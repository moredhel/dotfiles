Config { font = "xft:Terminus:size=8:antialias=true"
       , bgColor = "#292929"
       , fgColor = "#aeafb5"
       , position = TopW C 100
       , lowerOnStart = True
       , commands = [ Run Weather "EGPH" ["-t","<fc=#67b0ea><skyCondition></fc> <tempC>","-L","5","-H","15","--normal","#aeafb5","--high","#e3be00","--low","#67b0ea"] 36000
                    , Run Network "eth0" ["-L","32","-H","256","--normal","green","--high","red","--low","#67b0ea"] 10 -- add template
                    , Run Cpu ["-L","15","-H","50","--normal","green","--high","red","--low","#67b0ea"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b%_d %H:%M" "date" 10 --wierd format error is here > 10
                    , Run Uptime ["-t","<days>d <hours>h"] 601
                    , Run MPD ["-t", "<title> . <fc=#aeafb5><album></fc> - <fc=#aeafb5><artist></fc> <fc=#67b0ea><lapsed></fc>.<fc=#67b0a0><remaining></fc> <fc=#00d2ff><statei></fc>"] 10
                    , Run Com "/home/moredhel/bin/vol" [] "vol" 10
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%UnsafeStdinReader% } %cpu% | %memory% * %swap% | %eth0% { <action=`mpc toggle` button=1><action=`~/bin/vol +` button=4><action=`~/bin/vol -` button=5>%mpd%</action></action></action> [<fc=#67b0a0>%vol%</fc>] <fc=#aeafb5>%uptime%</fc> . %EGPH% . <fc=#aeafb5><action=`orage -t`>%date%</action></fc>"
       }
