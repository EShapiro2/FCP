set autolist
set history=100
set savehist=100
set noclobber
set notify
set filec
set mail = (5 /usr/spool/mail/$USER )
set path = ($path /usr/local/bin /usr/local/bin/X11R5 /usr/local/X11R5/bin)
if ( ! $?prompt ) exit 
set lnamehost = `hostname`
set namehost = `echo $lnamehost | /usr/bin/awk -F. '{print $1}'`
if ($shell == "/bin/csh") then
  set prompt="`echo $namehost`:$cwd->\!% "
endif
if ($shell == "/usr/local/bin/tcsh" || $shell == "/bin/tcsh") then
  set prompt="%m:%~->%h% "
endif
switch ("$TERM")
   case sun:
      breaksw
   case xterm:
      /X11/bin/resize > /dev/null
      breaksw
   case iris-ansi:
      /X11/bin/resize > /dev/null
      breaksw
   case dialup:
   case vt100:
   case dumb:
   default:
      unsetenv DISPLAY
      breaksw
endsw

setenv HOST `hostname`
source $HOME/.aliases
setenv ENSCRIPT '-fCourier10'
setenv LESS -ms
setenv PAGER more
setenv TEXEDIT "emacs +%d %s"
setenv TEXINPUTS .:$HOME/macros:/Tex/Macros
setenv EDITOR emacs
setenv PRINTER laser11
setenv NNTPSERVER news.weizmann.ac.il
setenv VISUAL /usr/local/bin/emacs
setenv DEFAULT_FONT /usr/lib/fonts/fixedwidthfonts/screen.r.12
setenv WWW_HOME http://www.wisdom.weizmann.ac.il/
setenv http_proxy http://wwwproxy.weizmann.ac.il:8080/
setenv no_proxy ac.il
setenv MOZILLA_HOME /usr/local/netscape

source ~/.aliases
