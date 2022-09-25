#
# /etc/bash.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

shopt -s no_empty_cmd_completion
shopt -s histappend
# for traversing
# shopt -s cdspell
# shopt -s autocd
# shopt -s globstar

PROMPT_COMMAND='history -a'

PS1='[\u@\h \W]\$ '
# Change the window title of X terminals 
case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

    ;;
  screen*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
    ;;
esac

if [ "$COLUMNS" -lt 117 ]; then
    echo -e "\e[37;1m         ___.                        .___                " #           .__   .__       .__                              "
    echo -e "\e[34;1m  _____  \_ |__  _____     ____    __| _/ ____    ____   " #   _____   |  |  |  |      |  |__    ____  ______    ____   "
    echo -e "\e[32;1m  \__  \  | __ \ \__  \   /    \  / __ | /  _ \  /    \  " #   \__  \  |  |  |  |      |  |  \  /  _ \ \____ \ _/ __ \  "
    echo -e "\e[32;1m   / __ \_| \_\ \ / __ \_|   |  \/ /_/ |(  <_> )|   |  \ " #    / __ \_|  |__|  |__    |   Y  \(  <_> )|  |_> >\  ___/  "
    echo -e "\e[95;1m  (____  /|___  /(____  /|___|  /\____ | \____/ |___|  / " #   (____  /|____/|____/    |___|  / \____/ |   __/  \___  > "
    echo -e "\e[37;1m       \/     \/      \/      \/      \/             \/  " #        \/                      \/         |__|         \/  "
else
    echo -e "\e[37;1m         ___.                        .___                           .__   .__       .__                              "
    echo -e "\e[34;1m  _____  \_ |__  _____     ____    __| _/ ____    ____      _____   |  |  |  |      |  |__    ____  ______    ____   "
    echo -e "\e[32;1m  \__  \  | __ \ \__  \   /    \  / __ | /  _ \  /    \     \__  \  |  |  |  |      |  |  \  /  _ \ \____ \ _/ __ \  "
    echo -e "\e[32;1m   / __ \_| \_\ \ / __ \_|   |  \/ /_/ |(  <_> )|   |  \     / __ \_|  |__|  |__    |   Y  \(  <_> )|  |_> >\  ___/  "
    echo -e "\e[95;1m  (____  /|___  /(____  /|___|  /\____ | \____/ |___|  /    (____  /|____/|____/    |___|  / \____/ |   __/  \___  > "
    echo -e "\e[37;1m       \/     \/      \/      \/      \/             \/          \/                      \/         |__|         \/  "
fi

[[ "$PS1" ]] && /usr/bin/fortune | while read line; do if [ -z "${line}" ]; then echo "$line"; else echo -e " \e[39;1m>> \e[90;1m$line \e[39;1m<<"; fi done

use_color=false
if type -P dircolors >/dev/null ; then
 # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
 LS_COLORS=
 if [[ -f ~/.dir_colors ]] ; then
  eval "$(dircolors -b ~/.dir_colors)"
 elif [[ -f /etc/DIR_COLORS ]] ; then
  eval "$(dircolors -b /etc/DIR_COLORS)"
 else
  eval "$(dircolors -b)"
 fi
 # Note: We always evaluate the LS_COLORS setting even when it's the
 # default.  If it isn't set, then `ls` will only colorize by default
 # based on file attributes and ignore extensions (even the compiled
 # in defaults of dircolors). #583814
 if [[ -n ${LS_COLORS:+set} ]] ; then
  use_color=true
 else
  # Delete it if it's empty as it's useless in that case.
  unset LS_COLORS
 fi
else
 # Some systems (e.g. BSD & embedded) don't typically come with
 # dircolors so we need to hardcode some terminals in here.
 case ${TERM} in
 [aEkx]term*|rxvt*|gnome*|konsole*|screen|cons25|*color) use_color=true;;
 esac
fi

if ${use_color} ; then
# if [[ ${EUID} == 0 ]] ; then
#  PS1='\[\033[01;31m\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
# else
#  PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
# fi
PS1='\n'
PS1+='\[\e[36;1m\]\u'                # Show username in blue + bold
PS1+='\[\e[36;1m\]@'                # Show @ in blue + bold
PS1+='\[\e[36;1m\]\h'               # Show hostname in blue + bold
PS1+='\[\e[32;1m\]:'                # Show : in green + bold WorkingDir $ in Green + Bold
PS1+='\[\e[32;1m\]\W'               # Show WorkingDir in green + bold 
PS1+='\[\e[32;1m\]\$'               # Show $ in green + bold
PS1+="\[\e[33;1m\]\$(parse_git_branch)"     # Show current git branch in yellow, if there's one at all
PS1+='\[\e[39;0m\] '                # Give it a space and make input text white and not bold
PS1+='\n\033[31;1m${?#0}\033[0;97m> \033[0m'

 #BSD#@export CLICOLOR=1
 #GNU#@alias ls='ls --color=auto'

else
 # show root@ when we don't have colors
 # PS1+='\u@\h \w \$ '
 PS1="\u@\h $(if [[ ${EUID} == 0 ]]; then echo '\W'; else echo '\w'; fi) \$([[ \$? != 0 ]] && echo \":( \")\$ "
fi


while read -r i; do
	aliasargs+=("$i")
done << EOF
    cp=cp --verbose --interactive
    mv=mv --verbose --interactive
    rm=rm --verbose --interactive
    ln=ln --verbose --interactive
    htop=htop --tree
    egrep=egrep --colour=auto
    fgrep=fgrep --colour=auto
    ls=ls --color=auto
    dir=dir --color=auto
    grep=grep --color=auto
    dmesg=dmesg --color
    bc=bc -l
    trash=gio trash
    timestamp=date +%s
    :q=exit
    xcpsel=xclip -sel clip
    dotadd=yadm add
    dotupdate=yadm add -u
    dotcommit=yadm commit -m "\$(timestamp)"
    dotpush=GIT_SSH_COMMAND="ssh -o IPQoS=throughput -i /home/abandon/system/git/keys/sgtrusty_rsa" yadm push
    updatemirrors=sudo reflector --verbose --latest 200 --protocol http --protocol https --sort rate --save /etc/pacman.d/mirrorlist
    applist=echo AppList -- From official repos: && pacman -Qe && echo AppList -- From Arch User Repository: && pacman -Qm
    nvusage=watch -n 0.5 nvidia-smi
    sockets=ss -tulpne
    cclean=sudo paccache -rk3 && sudo pacman -Sc --noconfirm && yay -Sc --noconfirm
    psg=ps aux | grep -v grep | grep -i -e VSZ -e
    hdmi=optimus-manager --switch hybrid && sudo pkill lightdm
    hdmi_bg=xrandr --output HDMI-1-0 --right-of eDP-1 --mode 1920x1080 --auto && exec feh --bg-fill Pictures/wallpaper/wallhaven-y8oqgl.png
EOF
alias "${aliasargs[@]}"
unset aliasargs

for sh in /etc/bash/bashrc.d/* ; do
 [[ -r ${sh} ]] && source "${sh}"
done

# Try to keep environment pollution down, EPA loves us.
unset use_color sh

alias sudo='sudo ' ## safe calls to sudo commands alias'ed above
#alias sudo='doas ' ## future sudo replacement
alias rm='echo "tip: use trash next time"; rm --verbose --interactive' ## we can disable this, already alias'ed up there anyway

# adding hidden file searcher for fzf
export FZF_DEFAULT_COMMAND="find -L"
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --inline-info'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview 'bat --color=always --line-range :500 {}' --preview-window=right:60%:wrap"
export FZF_ALT_C_COMMAND='fd --type d . $HOME'
export FZF_ALT_C_OPTS="--preview='cat {}' --preview-window=right:60%:wrap --preview 'tree -C {} | head -200'"
# making sure installed apps dont screw me over
export EDITOR=vim

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

# functions
## move this to another file sometime
function aprint() { awk '{print $1}'; }
function aprinta() { awk '{print $2}'; }
function aprintb() { awk '{print $2}'; }
function aprintc() { awk '{print $3}'; }
fzfyay () {
  yay -Slq | fzf -m --preview 'cat <(yay -Si {1}) <(yay -Fl {1} | awk "{print \$2}")' | xargs -ro  yay -S
}
parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
# Query package info
function query {
    case "$1" in
        se)
            pacman -Slq | fzf -q "$2" --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S
            ;;
        sel)
            pacman -Qq | fzf -q "$2" --multi --preview 'pacman -Qi {1}' | xargs -ro pacman -Qs
            ;;
        inf)
            yay -Si $2
            ;;
        infl)
            pacman -Qi $2
            ;;
        f2p)
            find // -type f,d,l -print0 2> /dev/null | fzf -q "$2" --preview 'bat -f {} 2> /dev/null || ls -lA --color=always {}' --read0 --print0 | xargs -r --null pacman -Qo
            ;;
        p2f)
            local file_list=$(pacman -Qsq | fzf -q "$2" --preview 'pacman  -Qi {1}' | xargs -r pacman -Qlq)
            if [ -n "$file_list" ]; then
                echo "$file_list" | fzf --multi --preview 'bat -f {} 2> /dev/null || ls -lA --color=always {}' | xargs -r echo
            fi
            ;;
        orphans)
            local package_list=$(pacman -Qdtq)
            if [ -n "$package_list" ]; then
            echo "$package_list" | fzf -q "$2" --multi --preview 'pacman -Qi {1}' | xargs -r pacman -Qi
            fi
            ;;
        explicit)
            pacman -Qetq | fzf -q "$2" --multi --preview 'pacman -Qi {1}' | xargs -r pacman -Qi
            ;;
        deps)
            pacman -Qdq | fzf -q "$2" --multi --preview 'pacman -Qi {1}' | xargs -r pacman -Qi
            ;;
        *)
            echo "Unknown command. Available options: se sel inf infl f2p p2f deps explicit orphans"
            # echo "Unknown command. Available options: se sel inf infl f2p p2f orphans explicit"
    esac
}
function yt2mp3 {
    if [ $# -ne 2 ]; then
        echo "First and second parameter must be set!"
        echo "(Usage: yt2mp3 <video-id> <folder/to/store/in>)"
        return
    fi
    
    local base_path="$2"
    if [[ "$base_path" != */ ]]; then
        base_path="$base_path/"
    fi
 
    echo "youtube-dl --newline -i --restrict-filenames -o '$base_path%(title)s.%(ext)s' -x --audio-format mp3 --audio-quality 0 --embed-thumbnail https://www.youtube.com/watch?v=$1"
    youtube-dl --newline -i --restrict-filenames -o "$base_path%(title)s.%(ext)s" -x --audio-format mp3 --audio-quality 0 --embed-thumbnail https://www.youtube.com/watch?v=$1 | less
}
# Display help for this bashrc
function bashrc {
    local fnc_desc=" \u26AC \e[38;5;4m%s\e[0m %s\n    \e[38;5;245m%s\e[0m\n"
    local fnc_desc_long="\t-> \e[38;5;4m%s\e[0m %s\n\t   \e[38;5;245m%s\e[0m\n"
    printf '\e[1mThis custom bashrc offers the following functions:\e[0m\n'
    printf "$fnc_desc" "nvusage" "" "Show live usage of NVIDIA graphics card"
    # printf "$fnc_desc" "expl" "<package name> ..." "Mark packages as explicitly installed (standalone)"
# alias expl="sudo pacman -D --asexplicit"
    # printf "$fnc_desc" "asdeps" "<package name> ..." "Mark packages as dependency"
# alias asdeps="sudo pacman -D --asdeps"
# alias pkgi="makepkg -sicr"
#     printf "$fnc_desc" "pkgi" "" "Install PKGBUILD in current folder"
# alias pkgb="makepkg -scr"
#     printf "$fnc_desc" "pkgb" "" "Build PKGBUILD in current folder"
    printf "$fnc_desc" "cclean" "" "Remove cached packages"
    printf "$fnc_desc" "psg" "<process name>" "Search for a currently running process with specified name"
    printf "$fnc_desc" "sockets" "" "Show all currently open tcp and udp sockets"
    printf "$fnc_desc" "yt2mp3" "<video-id> <path/to/ouputfolder>" "Download youtube video and convert to mp3 file"
    printf "$fnc_desc" "query" "<modifier> [optional search term]" "Package querying functions"
    printf "$fnc_desc_long" "query se" "[package name]" "Search for pacman/AUR packages online"
    printf "$fnc_desc_long" "query sel" "[package name]" "Search for locally installed packages"
    printf "$fnc_desc_long" "query inf" "[package name]" "Display information of a specified pacman/AUR package"
    printf "$fnc_desc_long" "query infl" "[package name]" "Display information of a specified locally installed package"
    printf "$fnc_desc_long" "query f2p" "[path/to/file]" "Display the package which belongs to a specified file"
    printf "$fnc_desc_long" "query p2f" "[package name]" "Display all files that belong to a package"
    printf "$fnc_desc_long" "query orphans" "[package name]" "Display all orphaned packages"
    printf "$fnc_desc_long" "query explicit" "[package name]" "Display all explicitly installed packages"
    printf "$fnc_desc_long" "query deps" "[package name]" "Display all packages that are marked as dependency"
    printf "\n"
}
# http://mywiki.wooledge.org/BashFAQ/037
colors256() {
        local c i j

        printf "Standard 16 colors\n"
        for ((c = 0; c < 17; c++)); do
                printf "|%s%3d%s" "$(tput setaf "$c")" "$c" "$(tput sgr0)"
        done
        printf "|\n\n"

        printf "Colors 16 to 231 for 256 colors\n"
        for ((c = 16, i = j = 0; c < 232; c++, i++)); do
                printf "|"
                ((i > 5 && (i = 0, ++j))) && printf " |"
                ((j > 5 && (j = 0, 1)))   && printf "\b \n|"
                printf "%s%3d%s" "$(tput setaf "$c")" "$c" "$(tput sgr0)"
        done
        printf "|\n\n"

        printf "Greyscale 232 to 255 for 256 colors\n"
        for ((; c < 256; c++)); do
                printf "|%s%3d%s" "$(tput setaf "$c")" "$c" "$(tput sgr0)"
        done
        printf "|\n"
}
colors8() {
        local c i j

        echo "Shell-std colors\n"
        for ((c = 0; c < 107; c++)); do
                printf "\e[$c;1m $c"
        done
        printf "\e[0;0m"
}