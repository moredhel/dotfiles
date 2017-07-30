#!/bin/bash

# load Xresources
#[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Dbus fix
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

# Gtk themes 
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
#settings for mail cron-job
touch $HOME/.Xdbus
chmod 600 $HOME/.Xdbus
env | grep DBUS_SESSION_BUS_ADDRESS > $HOME/.Xdbus
echo 'export DBUS_SESSION_BUS_ADDRESS' >> $HOME/.Xdbus
#----------------

#setxkbmap dvorak
setxkbmap -layout "us" -variant dvp -option "grp:caps_toggle"
setxkbmap -option terminate:ctrl_alt_bksp
#xset -dpms
xset s off
nm-applet &
(sleep 1 && parcellite &)
mpd &
urxvtd &
thunar --daemon &
unclutter -noevents &
dropboxd &
#conky -c ~/.xmonad/conky.conf &
trayer --edge bottom --align right --SetDockType true --SetPartialStrut false \
 --expand true --width 9 --transparent true --tint 0x000000 --height 10 &
xsetroot -cursor_name left_ptr &
xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &
#bash /home/hamhut/bin/wall.sh &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
~/bin/wallpaper.sh &
#exec startxfce4
exec urxvtc -e ncmpcpp &
#exec xfce4-session &
exec rox -S &
sleep 10 && exec thunderbird &
sleep 60 && exec gnucash &
sleep 3 && exec dwb &
exec xmonad
#exec i3 -V -d all >> ~/.i3/logfile
