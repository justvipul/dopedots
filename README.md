# mxtest-dope-dots

![doperice1](.screenshot/1.png)
![doperice2](.screenshot/3.png)




## Initial step
```
mkdir ~/gitall
mkdir ~/pix
mkdir ~/pix/wall
mkdir ~/dur
cd ~/gitall
git clone https://gitlab.com/only_vip/mxtest-dope-dots.git
```
## Things to delete from mx after first install
```
sudo aptitude remove vim-tiny vim-common asunder bluetooth clementine gimp gimp-data gimp-python libgimp2.0 simple-scan gscan2pdf hexchat lbreakout2 gnome-mahjongg mc mc-data nomacs nomacs-l10n thunderbird transmission-gtk transmission-common xfburn
```
## Things to install later
### Fonts
```
sudo aptitude install fonts-noto-color-emoji fonts-noto-color-emoji fonts-symbola node-emojis-list ttf-ancient-fonts-symbola fonts-fork-awesome nordic-themes fonts-powerline fonts-roboto fonts-roboto-fontface fonts-ubuntu ttf-ubuntu-font-family fonts-font-awesome fonts-fork-awesome fonts-material-design-icons-iconfont
```
### Dependencies and apps
```
sudo aptitude install vim cmake  bspwm sxhkd lxappearance polybar fzf evince w3m w3m-img youtube-dl lolcat arandr nitrogen sxiv mpv x11-utils mpd mpc ncmpcpp pylint dmenu netcat jq ffmpeg caca-utils chafa libsixel1 flameshot libsixel-bin qbittorrent uget qutebrowser rofi rofi-calc network-manager git curl wget tree libreadline-dev xattr zathura zathura-cb zathura-djvu zathura-pdf-poppler zathura-ps python3-setuptools python3-dev python3-pip atool rar moc mediainfo exiftool odt2txt rtorrent python3-wheel python3-docopt rxvt-unicode-256color tmux python3-ueberzug

```
## setup [MPR](https://mpr.hunterwittenborn.com/packages/mpm)
>First, add the signing key:

```
    wget -qO - 'https://proget.hunterwittenborn.com/debian-feeds/makedeb.pub' | gpg --dearmor | sudo tee /usr/share/keyrings/makedeb-archive-keyring.gpg &> /dev/null

```
>Next, add the repository information to your system:

```
echo 'deb [signed-by=/usr/share/keyrings/makedeb-archive-keyring.gpg arch=all] https://proget.hunterwittenborn.com/ makedeb main' | sudo tee /etc/apt/sources.list.d/makedeb.list

```
>Lastly, update the repository cache on your system:

```
sudo apt update && sudo apt install makedeb-beta
```

install from mpr




1. compton
```
git clone https://mpr.hunterwittenborn.com/compton-tryone-git.git
cd ~/dur/compton-tryone-git
makedeb -si
```
2. picom
```
git clone https://mpr.hunterwittenborn.com/picom-jonaburg-git.git
cd ~/dur/picom-jonaburg-git
makedeb -si
```
3. fzf-tab-completion
```
git clone https://mpr.hunterwittenborn.com/fzf-tab-completion-git.git
cd ~/dur/fzf-tab-completion-git
makedeb -si
```
4. shell-color-scripts
```
git clone https://mpr.hunterwittenborn.com/shell-color-scripts.git
cd ~/dur/shell-color-scripts
makedeb -si
```
5. nnn
```
git clone https://mpr.hunterwittenborn.com/nnn-git.git
cd ~/dur/nnn-git
makedeb -si
```
6. starship-prompt
```
git clone https://mpr.hunterwittenborn.com/starship-bin.git
cd ~/dur/starship-bin
makedeb -si
```
7. fzf
```
git clone https://mpr.hunterwittenborn.com/fzf-git.git
cd fzf-git
makedeb -si
```
8. dunst
```
git clone https://mpr.hunterwittenborn.com/dunst-git.git
cd ~/dur/dunst-git
makedeb -si
```
9. ytfzf
```
git clone https://mpr.hunterwittenborn.com/ytfzf.git
cd ~/dur/ytfzf
makedeb -si
```
10. bat-cat
```
git clone https://mpr.hunterwittenborn.com/bat-cat-bin.git
cd ~/dur/bat-cat-bin
makedeb -si
```
11. exa
```
git clone https://mpr.hunterwittenborn.com/exa-bin.git
cd ~/dur/exa-bin
makedeb -si
```
12. neofetch
```
git clone https://mpr.hunterwittenborn.com/neofetch-git.git
cd ~/dur/neofetch-git
makedeb -si
```
13. oranchelo icon theme  
```
git clone https://mpr.hunterwittenborn.com/oranchelo-icon-theme.git
cd ~/dur/oranchelo-icon-theme
makedeb -si
```    
14. rl-custom-git
```
git clone https://mpr.hunterwittenborn.com/rl-custom-function-git.git
cd ~/dur/rl-custom-function-git
makedeb -si
```
15. rofi
```
git clone https://mpr.hunterwittenborn.com/rofi.git
cd ~/dur/rofi
makedeb -si
```
16.mpm
```
git clone https://mpr.hunterwittenborn.com/mpm.git
cd mpm
makedeb -si
```






## Get a nerd font
[Nerd-fonts](https://www.nerdfonts.com/)

### libreoffice appimage
[Libreoffice-appimage](https://www.libreoffice.org/download/appimage/)



## Sid's st deps
```
sudo aptitude install libxft-dev libx11-dev libharfbuzz-dev libxext-dev libxrender-dev libxinerama-dev

```





## Copy rsfetch,viman and atomicparsley to /usr/bin
```
sudo cp ~/copytousrbinlocal/* /usr/bin/
```

### Reload font cache after copying the fonts folder to ~/.fonts/*

```
fc-cache -fv
```

## Get pywal,colorz,schemer2,ueberzug.

```
pip3 install pywal colorz
```


## Install papirus icon theme
### Papirus Installer

### Use the scripts to install the latest version directly from their github repo (independently of your distro):

##### NOTE: Use the same script to update icon themes.
### ROOT directory (recommended)
```
wget -qO- https://git.io/papirus-icon-theme-install | sh
```
### HOME directory for GTK
```
wget -qO- https://git.io/papirus-icon-theme-install | DESTDIR="$HOME/.icons" sh
```
### HOME directory for KDE
```
wget -qO- https://git.io/papirus-icon-theme-install | DESTDIR="$HOME/.local/share/icons" sh
```
### *BSD systems
```
wget -qO- https://git.io/papirus-icon-theme-install | env DESTDIR="/usr/local/share/icons" sh
```
### Uninstall
```
wget -qO- https://git.io/papirus-icon-theme-uninstall | sh
```
### Papirus-folder install
##### Use the script to install the latest version directly from their github repo (independently on your distro):

Install
```
wget -qO- https://git.io/papirus-folders-install | sh
```

#### To install papirus-folders on BSD systems using the following command:
```
wget -qO- https://git.io/papirus-folders-install | env PREFIX=/usr/local sh
```
#### Uninstall
```
wget -qO- https://git.io/papirus-folders-install | env uninstall=true sh
```




## Install rustup
```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

```
### change crontab entry to have wal run every 10 minutes
run `crontab -e` enter following command at the end
```
*/10 * * * * DISPLAY=:0 ~/.local/bin/wal -a "50" backend colorz -i $(find ~/wals/* | shuf | head -n 1)
```
### install ntfd
>install ntfd then change the `~/.config/polybar/config` module-right to include the weather module. 
### make tmux symlink
> make a symlink of the tmux config    `ln -s -f ~/.tmux/.tmux.conf ~/.tmux.conf`

### misc. links
[Bgra-debian](https://github.com/ra-c/libxft-bgra-debian)\
[nnn-livepreview](https://github.com/jarun/nnn/wiki/Live-previews)\
[For-art-downloading-in-ytdl,atomicparsley](https://github.com/wez/atomicparsley)\
[SACAD,SmartAutomaticCoverArtDownloader](https://github.com/desbma/sacad)\
[Termite-terminal-install-guide](https://epsi-rns.github.io/desktop/2016/09/19/termite-install.html)\
[vimwiki](https://cristianpb.github.io/vimwiki/st/)\
[ytfzf](https://github.com/pystardust/ytfzf)\
[Suckless-tabbed](https://tools.suckless.org/tabbed/)

### All github links
```
cd ~/gitall
git clone https://github.com/alexanderjeurissen/ranger_devicons
git clone https://github.com/siduck76/st
```

##### Remove this patch later

```
https://st.suckless.org/patches/bold-is-not-bright/
```

### ~~termite is deprecated but the install script is still here just for nostalgia~~
~~### termite install~~


mkdir ~/git-src
cd ~/git-src

sudo apt install gtk-doc-tools valac libgirepository1.0-dev libgtk-3-dev libgnutls28-dev intltool libxml2-utils gperf

git clone https://github.com/thestinger/vte-ng.git

cd vte-ng

git cherry-pick 53690d5c

./autogen.sh

make

sudo make install

cd ~/git-src
git clone --recursive https://github.com/thestinger/termite.git
cd termite
make
sudo make install

cd ~/git-src/vte-ng
sudo make uninstall~~

~~for saner working of termite~~

```
wget https://raw.githubusercontent.com/thestinger/termite/master/termite.terminfo
tic -x termite.terminfo
```
