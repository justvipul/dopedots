# mxtst-dope-dots
## things to delete from mx after first install
```
sudo aptitude remove vim-tiny vim-common asunder bluetooth clementine gimp gimp-data gimp-python libgimp2.0 simple-scan gscan2pdf hexchat lbreakout2 gnome-mahjongg mc mc-data nomacs nomacs-l10n thunderbird transmission-gtk transmission-common xfburn
```
## things to install later
### fonts
```
sudo aptitude install fonts-noto-color-emoji fonts-noto-color-emoji fonts-symbola node-emojis-list ttf-ancient-fonts-symbola fonts-fork-awesome nordic-themes fonts-powerline fonts-roboto fonts-roboto-fontface fonts-ubuntu ttf-ubuntu-font-family fonts-font-awesome fonts-fork-awesome
```
## dependencies and apps
```
sudo aptitude install vim cmake  bspwm sxhkd lxappearance polybar fzf evince w3m w3m-img youtube-dl lolcat arandr nitrogen sxiv mpv x11-utils mpd mpc ncmpcpp cargo cargo-doc dh-cargo cargo-lichking rustc pylint dmenu netcat jq ffmpeg caca-utils chafa libsixel1 flameshot libsixel-bin qbittorrent uget qutebrowser rofi rofi-calc network-manager git curl wget tree libreadline-dev xattr zathura zathura-cb zathura-djvu zathura-pdf-poppler zathura-ps python3-setuptools atool bsdtar rar moc mediainfo exiftool odt2txt rtorrent papirus-folders 

```


## compton dependencies
```
sudo aptitude install libx11-dev libxcomposite-dev libxdamage-dev libxfixes-dev libxext-dev libxrender-dev libxrandr-dev libxinerama-dev pkg-config make libpcre2-dev libconfig-dev libdrm-dev  libx11-dev libxcomposite-dev libxdamage-dev libxfixes-dev libxext-dev libxrender-dev libxrandr-dev libxinerama-dev pkg-config make libpcre2-dev libconfig-dev libdrm-dev libdbus-1-dev  libgl-dev libpcre++-dev docbook-xml libxslt1-dev xsltproc xmlto asciidoc-base asciidoc-common xsltproc libxcb-image0-dev libxcb-damage0-dev  libpixman-1-dev  dmenu asciidoc

```

## picom dependencies
```
sudo aptitude install meson libxext-dev libxcb1-dev libxcb-damage0-dev libxcb-xfixes0-dev libxcb-shape0-dev libxcb-render-util0-dev libxcb-render0-dev libxcb-randr0-dev libxcb-composite0-dev libxcb-image0-dev libxcb-present-dev libxcb-xinerama0-dev libpixman-1-dev libdbus-1-dev libconfig-dev libgl1-mesa-dev  libpcre2-dev  libevdev-dev uthash-dev libev-dev libpcre++-dev  libx11-xcb-dev
```

### all github links
```
git clone https://github.com/lincheney/fzf-tab-completion
git clone https://github.com/ranger/ranger
git clone https://github.com/alexanderjeurissen/ranger_devicons
git clone https://github.com/jarun/nnn
git clone https://github.com/ryanoasis/vim-devicons
git clone https://github.com/thestinger/termite
git clone https://github.com/jonaburg/picom
git clone https://github.com/tryone144/compton
git clone https://gitlab.com/dwt1/shell-color-scripts
git clone https://github.com/siduck76/st

```

remove this patch later

```
https://st.suckless.org/patches/bold-is-not-bright/
```
### [starship-prompt.rs](https://starship.rs/)

# install starship
`curl -fsSL https://starship.rs/install.sh | bash`

## get a nerd font
[Nerd-fonts](https://www.nerdfonts.com/)

### libreoffice appimage
[Libreoffice-appimage](https://www.libreoffice.org/download/appimage/)


## install nnn with icon fonts 
```
cd nnn 
make
sudo make O_NERD=1

```

## sids st deps
```
sudo aptitude install libxft-dev libx11-dev libharfbuzz-dev libxext-dev libxrender-dev libxinerama-dev

```

### termite install
```
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
sudo make uninstall

for saner working
wget https://raw.githubusercontent.com/thestinger/termite/master/termite.terminfo
tic -x termite.terminfo
```



### misc. links
[Bgra-debian](https://github.com/ra-c/libxft-bgra-debian)\
[nnn-livepreview](https://github.com/jarun/nnn/wiki/Live-previews)\
[For-art-downloading-in-ytdl,atomicparsley](https://github.com/wez/atomicparsley)\
[SACAD,SmartAutomaticCoverArtDownloader](https://github.com/desbma/sacad)\
[Termite-terminal-install-guide](https://epsi-rns.github.io/desktop/2016/09/19/termite-install.html)\
[vimwiki](https://cristianpb.github.io/vimwiki/st/)\
[ytfzf](https://github.com/pystardust/ytfzf)\
[Suckless-tabbed](https://tools.suckless.org/tabbed/)



