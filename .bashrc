#
# /etc/bash.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

shopt -s no_empty_cmd_completion
shopt -s histappend

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

[[ "$PS1" ]] && /usr/bin/fortune

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
 if [[ ${EUID} == 0 ]] ; then
  PS1='\[\033[01;31m\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
 else
  PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
 fi

 #BSD#@export CLICOLOR=1
 #GNU#@alias ls='ls --color=auto'
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
  dotupdate=yadm add -u
  dotcommit=yadm commit -m "\$(timestamp)"
EOF
alias "${aliasargs[@]}"
unset aliasargs


else
 # show root@ when we don't have colors
 # PS1+='\u@\h \w \$ '
 PS1="\u@\h $(if [[ ${EUID} == 0 ]]; then echo '\W'; else echo '\w'; fi) \$([[ \$? != 0 ]] && echo \":( \")\$ "
fi

for sh in /etc/bash/bashrc.d/* ; do
 [[ -r ${sh} ]] && source "${sh}"
done

# Try to keep environment pollution down, EPA loves us.
unset use_color sh

alias sudo='sudo ' ## safe calls to sudo commands alias'ed above
alias rm='echo "tip: use trash next time"; rm --verbose --interactive' ## we can disable this, already alias'ed up there anyway

# adding hidden file searcher for fzf
export FZF_DEFAULT_COMMAND="find -L"
# making sure installed apps dont screw me over
export EDITOR=vim

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

# functions
## move this to another file sometime
function aprint() { awk '{print $1}'; }
function aprinta() { awk '{print $2}'; }
function aprintb() { awk '{print $2}'; }
function aprintc() { awk '{print $3}'; }
function xcpsel() { xclip -sel clip; }
function :q() { exit; }

