# Source Download
1. https://nim-lang.org/install_unix.html
1. Extract the source tarball to `/tmp/`
1. term$>&emsp;`cd /tmp/`
1. Navigate to extracted folder
1. term$>&emsp;`sh build.sh`
1. Navigate to version folder
1. term$>&emsp;`bin/nim c koch`
1. term$>&emsp;`./koch boot -d:release`
1. term$>&emsp;`./koch tools`
1. term$>&emsp;`<editor> ~/.bashrc`
    * Make the below additions
1. term$>&emsp;`sudo mv nim-N.N.N /opt/nim-N.N.N` , Using the appropriate version
1. term$>&emsp;`sudo reboot now`
1. term$>&emsp;`nim --version`



# `~/.bashrc` Additions
``` 
PATH="/opt/nim-N.N.N/bin:$PATH" # Substitute the version that was installed
PATH="~/.nimble/bin:$PATH"
```