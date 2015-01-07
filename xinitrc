wmname LG3D
setxkbmap -layout us,es -variant altgr-intl, -option grp:rwin_toggle -option grp_led:scroll -option ctrl:nocaps
#export GVM_INIT=false

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
export GTM_IM_MODULE_FILE="/usr/lib/gtk-2.0/2.10.0/immodules.cache"
exec /home/niwi/dwm/dwm
#exec startkde
