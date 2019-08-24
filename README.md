# bakdotfiles.py
A Python 3.7+ script to backup config files ("dotfiles") on Manjaro/Arch-based Linux distributions.  
It parses the output of `pacman -Qkk` to know which files to backup, plus it accepts arbitrary glob patterns from a
configuration file to add/reject files from the backup.

## Usage
See [the example configuration file](./dotfiles.cfg), and run `bakdotfiles.py -h` for more information.

## License
[MIT](LICENSE).
