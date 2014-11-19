Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , bgColor = "#141314"
       , fgColor = "#a39b84"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Weather "EGPH" ["-t","<tempC>","-L","5","-H","15","--normal","#aeafb5","--high","#ad8d54","--low","#a6a89c"] 36000
                    , Run Network "enp3s0" ["-L","0","-H","32","--normal","#93744E","--high","#794742"] 10
                    , Run Cpu ["-L","10","-H","50","--low","#4f4247","--normal","#93744E","--high","#794742"] 10
                    , Run Memory ["-t","<usedratio>%", "-L","10", "-H","75","--low", "#4f4247","--normal","#93744E", "--high","#794742"] 10
                    , Run Swap [] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
       ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% } %cpu% | Mem: %memory% | %enp3s0% { %EGPH% | <fc=#794742>%date%</fc>"
       }
