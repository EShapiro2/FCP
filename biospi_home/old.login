#!/bin/csh
set path=( . $HOME $HOME/bin /usr/local/bin/ts /usr/local/bin /usr/local/bin/X11R5  /usr/bin/X11 /X11/bin /usr/ucb /usr/bin /bin /etc /usr/etc /usr/etc/yp /usr/hosts /etc/local )

switch ("$TERM")
   case vt220:
   case vt100:
   case vi100:
   case vt300:
   case dumb:
      set noglob; eval `tset -s -Q -I vt100 `; unset noglob
      unsetenv TERMCAP
      unsetenv DISPLAY
      set term=vt100
      breaksw
   case sun:
   case xterm:
   case iris-ansi:
      breaksw
   case dialup:
   default:
     tset vt100; setenv TERM vt100;set term=vt100; tset -e^H
endsw
echo "Terminal type was set to:" $TERM

sun4c && set machine_type="sun4c"
sun4m && set machine_type="sun4m"
sun4 && set machine_type="sun4"
mips && set machine_type="dec"
4d  && set machine_type="sgi"
rs6000  && set machine_type="ibm"
solaris  && set machine_type="solaris"
linux  && set machine_type="linux"
  switch ("$machine_type")
   case sun4:
     stty dec
     set noglob; eval `tset -s -Q -I `; unset noglob
     set path= ($path /usr/5bin /usr/openwin/bin /usr/lang)
     setenv LD_LIBRARY_PATH /usr/lib:/usr/openwin/lib:/usr/local/lib
     breaksw
   case sun4c:
     stty dec
     set noglob; eval `tset -s -Q -I `; unset noglob
     set path= ($path /usr/5bin /usr/openwin/bin /usr/lang /usr/demo/SOUND)
     setenv LD_LIBRARY_PATH /usr/local/ImageTools/lib:/usr/lib:/usr/openwin/lib:/usr/local/lib
     breaksw
   case dec:
     stty dec
     set noglob; eval `tset -s -Q -I `; unset noglob
     breaksw
   case sun4m:
     stty dec
     set noglob; eval `tset -s -Q -I `; unset noglob
     set path= ($path /usr/5bin /usr/openwin/bin /usr/lang /usr/demo/SOUND)
     setenv LD_LIBRARY_PATH /usr/local/ImageTools/lib:/usr/lib:/usr/openwin/lib:/usr/local/lib
     breaksw
   case solaris:
     stty dec
     set noglob; eval `tset -s -Q -I `; unset noglob
     set path= ($path /usr/ccs/bin /usr/sbin /usr/openwin/bin  /opt/SUNWspro/bin /usr/lang /usr/demo/SOUND)
     setenv LD_LIBRARY_PATH /usr/lib:/usr/openwin/lib:/usr/local/ImageTools/lib:/usr/local/lib
     setenv GS_FONTPATH /usr/openwin/lib/X11/fonts/Type1/outline
     setenv GS_LIB /usr/openwin/lib/X11/fonts/Type1:/usr/openwin/lib/X11/fonts/Type3
     setenv LM_LICENSE_FILE /opt/SUNWspro/licenses_dir/sunpro.lic,school
     breaksw
   case ibm:
     set path= ($path /usr/sbin /usr/local/X11R5/bin)
     if ($?prompt) then
       stty erase 
     endif
     set noglob; eval `tset -s -Q -I `; unset noglob
     breaksw
   case sgi:
     stty dec
     set path= ( $path /usr/bsd /usr/openwin/bin /usr/sbin )
     setenv LD_LIBRARY_PATH /usr/local/ImageTools/lib:/usr/lib:/usr/local/lib
     setenv LD_LIBRARYN32_PATH /usr/lib32/mips3:/usr/lib32/mips4:/usr/lib32
     setenv LD_LIBRARY64_PATH /usr/lib64/mips3:/usr/lib64/mips4:/usr/lib64
     alias mail Mail
     breaksw
   case linux:
     setenv LD_LIBRARY_PATH /usr/local/ImageTools/lib:/usr/lib:/usr/local/lib
     breaksw
   default: 
       echo "No such machine at Wisdom"
endsw

set host_name=`hostname`
limit core 0

set ttyname=`tty`

if ("$ttyname" == "/dev/console" && "$TERM" == "sun") then
        unalias vi
        echo ""
        echo -n "xinit? enter '[y],n' "
        set doit=$<
        if ("$doit" != "n") then
               setenv DISPLAY "$namehost":0
               if (-f .xinit_err) \rm .xinit_err
               echo $DISPLAY >& .xinit_err 
	       xinit >>& .xinit_err
               /X11/bin/kbd_mode -a 
               tput reset
               click -n
               unsetenv DISPLAY
               logout
        endif
endif
if ("$term" == "vt100" ) then 
   msgs
endif
#biff n
echo $HOST
#umask 022
umask 077
