# Terminal
1. term$>&emsp;`cd /tmp/`
1. term$>&emsp;`wget https://github.com/dom96/choosenim/releases/download/v0.8.2/choosenim-0.8.2_linux_amd64`
    * Choose new version as needed
1. term$>&emsp;`curl https://nim-lang.org/choosenim/init.sh -sSf | sh`
1. term$>&emsp;`nano ~/.bashrc`
    * Make the bash profile additions noted below.
1. term$>&emsp;`sudo reboot now`
1. term$>&emsp;`nim -v`
    * After reboot verify the latest stable Nim with this command.

# `~/.bashrc` Additions
``` 
export PATH=/home/jwatson/.nimble/bin:$PATH
```