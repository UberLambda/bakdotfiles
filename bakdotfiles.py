#!/usr/bin/env python3
# coding: utf-8
# vim: set sw=4 expandtab colorcolumn=120:

# The MIT License
# 
# Copyright (c) 2019 Paolo Jovon <paolo.jovon@gmail.com>
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

'''Parses `pacman -Qkk` output to find modified package files, then makes backup copies of them
(including/excluding any file specified in user-defined glob patterns)'''


import subprocess as sp
import re
import os
import sys
import shutil
import fnmatch, glob
from collections import namedtuple, defaultdict, OrderedDict
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


PKG_MANAGER = 'pacman'  # = 'yay'
'''The pacman-like package manager to use.'''

WARNING_RE = re.compile(r'(?:warning|backup file): [^\s:]+: ([^\s]+) \(([^\)]*)\)$', re.MULTILINE)
'''Matches a filename and warning message from `pacman -Qkk` output.'''

IGNORED_WARNINGS = ['Permission denied', 'Permissions mismatch']
'''List of `pacman -Qkk` warnings that do not trigger a backup for a file.'''

SELF_DIR = os.path.dirname(os.path.abspath(__file__))
'''The location of this script.'''

BACKUP_DIR = os.path.join(SELF_DIR, 'backup')
'''Path to the default directory to backup files to.'''

CFG_FILE = os.path.join(SELF_DIR, 'dotfiles.cfg')
'''Path to the default backup configuration file.'''

OLD_SUFFIX = '.bakold'
'''Suffix to add to the filenames of outdated files.'''


class Log:
    TRACE = -1
    INFO = 0
    WARN = 1
    ERROR = 2

    TAGS = {
        TRACE: '',
        INFO: '--',
        WARN: '(!)',
        ERROR: '[!]',
    }

    def __call__(self, level, *args, **kwargs):
        tag = self.TAGS.get(level, '')
        print(tag, *args, file=sys.stderr, **kwargs)

log = Log()


dictree = lambda: defaultdict(dictree) # Magic!

class GlobMatcher:
    '''Builds a regular expression that matches any of the added glob patterns'''

    glob_substs = OrderedDict([
        (r'\\\*\\\*', r'.*'),
        (r'\\\*', r'[^/]*'),
        (r'\\\?', r'[^/]'),
    ])
    glob_subber = re.compile('|'.join(glob_substs.keys()))

    def __init__(self):
        self.tree = dictree()  
        '''Basically a trie in which each node is a path node.
        '/a/**/b' becomes:
        {
            'a': {
                '**': {
                    'b': None,
                }
            }
        }
        '''

    def _glob_sub(self, glob_node):
        '''Translates a glob path node (ex. 'a', 'foo*.bar', '**') to a regex.'''

        regex = re.escape(glob_node)
        regex = self.glob_subber.sub(lambda match: self.glob_substs[re.escape(match.group())], regex)
        return f'(?:{regex})'

    @staticmethod
    def normalize_glob(glob_pat):
        '''Normalizes a glob pattern, making it start at / and normalizing paths within it.'''

        if glob_pat.endswith('/') or os.path.isdir(glob_pat):
            # If the pattern ends with a trailing slash, or if we know for sure it refers to a folder,
            # it must be for a folder; match all of its children
            glob_pat = glob_pat[:-1] + '**'

        # Remove duplicated slashes, '.' and '..'
        glob_pat = os.path.normpath(str(glob_pat))

        if glob_pat.startswith('**'):
            # Make the pattern start at the root directory
            glob_pat = '/' + glob_pat
        elif glob_pat.startswith('*'):
            # Ditto
            glob_pat = '/*' + glob_pat
        elif glob_pat.startswith('/'):
            # The pattern already starts at the root directory, nothing to do.
            pass
        else:
            # Relative glob patterns make little sense in this context; disallow them
            raise RuntimeError('Invalid glob pattern: {glob_pat} (it should start with / or *)')

        return glob_pat

    def add(self, glob_pat):
        '''Adds `glob_pat` to the list of patterns to match (after normalizing it).'''

        glob_pat = self.normalize_glob(glob_pat)
        assert len(glob_pat) != 0

        nodes = glob_pat.split('/')
        nodes = nodes[1:]  # No need for the first, empty node

        tree = self.tree
        for node in nodes[:-1]:
            tree = tree[node]
        tree[nodes[-1]] = None

    def _build_node_regex(self, node):
        '''Returns a regex build for `node`, a node of `self.tree`.'''

        regex = '(?:'
        for i, (k, v) in enumerate(node.items(), start=1):
            regex += self._glob_sub(k)
            if type(v) == type(self.tree):
                regex += r'\/'
                regex += self._build_node_regex(v)
            # else: leaf node
            if i != len(node): regex += '|'
        regex += ')'
        return regex

    def build_regex(self):
        '''Compiles a regex that will match a filepath if any of the `add()`ed glob patterns match it.'''

        regex = r'^\/' + self._build_node_regex(self.tree) + '$'
        return re.compile(regex)


def flatten_item(path):
    '''Given a path, yields (in this order):
    - All of the path's ancestor directories from root (but excluding /), in order
    - The path itself
    - All of the path's descendants (if path is a directory), in order'''

    # Yield all parent directories of the item in succession
    # This way `backup_file()` will mkdir() them before trying to copy their children
    root = os.path.dirname(os.path.normpath(path))
    sep_pos = 0
    while sep_pos < len(root):
        sep_pos = root.find('/', sep_pos + 1)
        if sep_pos < 0: sep_pos = len(root)
        yield root[:sep_pos]

    # Yield the item itself
    yield path

    # If `path` is a directory, walk though its children
    for root, dirnames, filenames in os.walk(path):
        yield root
        for dirname in dirnames: yield os.path.join(root, dirname)
        for filename in filenames: yield os.path.join(root, filename)

def changed_pkg_files():
    '''Calls `pacman(-like) -Qkk` to list all files/folders (items) that differ from the package's version.
    Returns a set containing `flatten_item(item) for item in changed_items`.'''

    cmd = [PKG_MANAGER, '-Qkk']
    try:
        cmd_output = sp.check_output(cmd, stderr=sp.STDOUT).decode('utf-8')
    except sp.CalledProcessError as err:
        # Exit code is non-zero: some file(s) probably changed in the package
        cmd_output = err.output.decode('utf-8')

    # Note that if the exit code is zero something could have changed in the package as well;
    # need to check the output to make sure

    warnings = WARNING_RE.findall(cmd_output)

    changed_files = set()
    for (warning_item, warning_msg) in warnings:
        if warning_msg not in IGNORED_WARNINGS:
            # If `warning_item` is a directory, it and all of its children must be listed recursively.
            # This is needed because otherwise blacklist glob patterns would not filter the children!
            changed_files.update(flatten_item(warning_item))

    return changed_files

def backup_item(src_path, backup_dir, force=False):
    '''Copies the item (file or directory) at `src_path` (an absolute path) to `backup_dir`. Preserves permissions.
    If `force`, the destination exists and it is not older than the source, raises a `FileExistsError`.'''

    # NOTE: os.path.join here would't do the trick!
    # NOTE: The parent directory of `dst_path` should already have been created! See `flatten_item()`
    dst_path = os.path.normpath(backup_dir + src_path)

    src_stat = os.stat(src_path)
    if not force:
        try:
            dst_stat = os.stat(dst_path)
            if dst_stat.st_mtime >= src_stat.st_mtime:
                raise FileExistsError(f"{dst_path} is not older than {src_path}")
        except FileNotFoundError:
            pass

    if os.path.isdir(src_path):
        try:
            os.mkdir(dst_path)
        except FileExistsError:
            raise
    else:
        # NOTE: The destination directory should already have been created!
        shutil.copy2(src=src_path, dst=dst_path)

    os.chown(dst_path, src_stat.st_uid, src_stat.st_gid)

def mark_old_files(src_root, bak_root, dry_run=False, verbose=False):
    '''Adds a suffix to the names of all files present in `bak_root` but not in `src_root`.'''
    for root, dirnames, filenames in os.walk(bak_root):
        rel_root = os.path.relpath(root, bak_root)
        for filename in filenames:
            if filename.endswith(OLD_SUFFIX):
                # Already marked as outdated
                continue

            bak_path = os.path.join(root, filename)
            src_path = os.path.join(src_root, rel_root, filename)
            if os.path.exists(src_path):
                continue

            if verbose:
                log(log.TRACE, src_path, 'is outdated')
            if not dry_run:
                os.rename(bak_path, bak_path + OLD_SUFFIX)

Config = namedtuple('Config', 'included excluded')

def read_cfg(stream):
    '''Reads +glob (included) and -glob (excluded) patterns from a configuration file.'''

    cfg = Config(included=set(), excluded=set())
    lines = stream.readlines()

    for n, line in enumerate(lines, start=1):
        if not line or line.isspace() or line.startswith('#'):
            continue

        splits = line.strip().split(maxsplit=1)
        if len(splits) == 2:
            op, pat = splits
            pat = pat.rstrip() # Prevent trailing whitespace in the pattern!
            if op == '+':
                cfg.included.add(pat)
                continue
            elif op == '-':
                cfg.excluded.add(pat)
                continue

        raise RuntimeError(f'Malformed config file: line {n} invalid: "{line}"')

    return cfg


def progress_foreach(items, operation, mk_start_msg, mk_item_msg, mk_done_msg, clear=True):
    '''1. Logs `mk_start_msg()`.
    2. For each item in `items`, logs `mk_item_msg(item_n=1.., item)` and calls `operation(item)`.
       If `clear` any previous item message line is cleared and the new one replaces it.
    3. Logs `mk_done_msg()`.
    '''
    
    log(log.INFO, mk_start_msg())

    for i, item in enumerate(items, start=1):
        progress_str = f'[{i}/{len(items)}]'
        item_msg = mk_item_msg(i, item)
        if clear:
            sys.stderr.write(f'\r\033[2K{progress_str} {item_msg:.100}')
        else:
            log(log.TRACE, progress_str, item_msg)

        operation(item)

    if clear: sys.stderr.write('\r\033[2K')
    log(log.INFO, mk_done_msg())

def path_item_msg(i, item):
    '''A `mk_item_msg()` for an item's path.'''

    prefix = '(directory) ' if os.path.isdir(item) else ''
    return f'{prefix}{item}'

def print_excluded_files(all_files, selected_files):
    '''Logs all items that are in `all_files` but not in `selected_files`.'''

    excluded = set(all_files) - set(selected_files)
    log(log.INFO, len(all_files), 'file(s) found;', len(excluded), 'of those will be excluded from backup:')
    for fpath in excluded:
        log(log.TRACE, '  ', fpath)

def parse_args():
    '''Interprets and returns shell arguments.'''

    parser = ArgumentParser(description=__doc__, formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-v', '--verbose',
            action='count', default=0,
            help='Increases the verbosity level of the output. Can be specified multiple times')
    parser.add_argument('-c', '--config',
            default=CFG_FILE,
            help='The backup configuration file to read')
    parser.add_argument('-O', '--backup-dir',
            default=BACKUP_DIR,
            help='The directory to backup files to')
    parser.add_argument('-n', '--dry-run',
            action='store_true', default=False,
            help='List files but do not actually back them up')
    parser.add_argument('-f', '--force',
            action='store_true', default=False,
            help='Copy files even if their backup is already present and not older than them')
    return parser.parse_args()

def main():
    '''Entry point.'''

    args = parse_args()

    if args.verbose >= 1:
        log(log.INFO, 'Backup destination:', args.backup_dir)

    try:
        if args.verbose >= 2:
            log(log.INFO, 'Reading config file:', args.config)
        with open(args.config) as cfg_file:
            cfg = read_cfg(cfg_file)
    except (PermissionError, FileNotFoundError, OSError) as e:
        log(log.ERROR, 'Could not open config file:', e)
        sys.exit(5)
    except RuntimeError as e:
        log(log.ERROR, e)
        sys.exit(4)

    if os.geteuid() != 0:
        log(log.WARN, 'Not running as root, may be unable to copy some files due to insufficient permissions!')
    if args.dry_run:
        log(log.WARN, 'Dry run: no files will actually be backed up!')

    clear_progress = args.verbose < 1
    clear_line = '\n' * clear_progress

    n_failed, n_skipped = 0, 0
    def do_backup_item(f):
        if args.dry_run:
            return
        try:
            backup_item(f, args.backup_dir, force=args.force)
        except FileExistsError as e: 
            if not clear_progress: log(log.WARN, f'\nskipped: {e}')
            nonlocal n_skipped; n_skipped += 1
        except Exception as e:
            log(log.WARN, f'{clear_line}failed to backup {f}: {e}')
            nonlocal n_failed; n_failed += 1

    # Build a regex that matches all files to exclude from the backup
    blacklist = GlobMatcher()
    for pat in cfg.excluded:
        blacklist.add(pat)

    blacklist_re = blacklist.build_regex()
    if args.verbose >= 2:
        log(log.INFO, 'Blacklist regex:', blacklist_re.pattern)

    # List changed package files
    log(log.INFO, 'Listing modified package files...')
    all_changed_files = changed_pkg_files()

    # Apply blacklist and sort files so that folders are created before their children
    changed_files = list(sorted(fpath for fpath in all_changed_files if not blacklist_re.match(fpath)))

    if args.verbose >= 2:
        print_excluded_files(all_changed_files, changed_files)
    else:
        log(log.INFO, len(changed_files), 'file(s) to backup found')

    # Ensure backup dir is present
    if not args.dry_run:
        os.makedirs(BACKUP_DIR, exist_ok=True)

    # Backup changed package files
    progress_foreach(changed_files, do_backup_item,
                     mk_start_msg=lambda: 'Backing up modified package files',
                     mk_item_msg=path_item_msg,
                     mk_done_msg=lambda: 'Done',
                     clear=clear_progress)

    # List user files
    log(log.INFO, 'Listing user files...')

    all_added_files = set()
    for glob_pat in cfg.included:
        if args.verbose >= 1:
            log(log.TRACE, '+', glob_pat)

        for globbed_item in glob.iglob(glob_pat, recursive=True):
            # If `globbed_item` is a directory, it and all of its children must be listed recursively.
            # Same reasoning as for `changed_pkg_files()`; otherwise, the blacklist globs would not filter the children!
            all_added_files.update(flatten_item(globbed_item))

    # Apply blacklist and sort files so that folders are created before their children
    added_files = list(sorted(fpath for fpath in all_added_files if not blacklist_re.match(fpath)))

    if args.verbose >= 2:
        print_excluded_files(all_added_files, added_files)
    else:
        log(log.INFO, len(added_files), 'file(s) to backup found')

    # Backup files from whitelist
    progress_foreach(added_files, do_backup_item,
                     mk_start_msg=lambda: f'Backing up user files',
                     mk_item_msg=path_item_msg,
                     mk_done_msg=lambda: 'Done',
                     clear=clear_progress)

    # Mark files present in the backup but not in the source as .old
    log(log.INFO, 'Detecting outdated files...')
    mark_old_files('/', args.backup_dir, args.dry_run, args.verbose >= 1)

    # Done
    if n_skipped > 0:
        log(log.INFO, n_skipped, 'file(s) were skipped')
    if n_failed > 0:
        log(log.ERROR, n_failed, 'file(s) could not be backed up')
        sys.exit(2)

    sys.exit(0)


if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
