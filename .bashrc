################ Set Prompt ######################
PS1="\h \w>"

################ History #########################
HISTSIZE=10000
HISTFILE=~/.bashhist                                                                                                                                                   
HISTFILESIZE=20000                                                                                                                                                              
HISTCONTROL=ignoreboth                                                                                                                                                           
shopt -s histappend                                                                                                                                                              
shopt -s checkwinsize 

################ Environment #####################
export PATH=$PATH:/usr/local/bin:/usr/X11R6/bin
export EDITOR=vim

################ Aliases #########################
alias ls="ls --color -aCF"

