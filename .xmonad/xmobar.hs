Config { font = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
       , bgColor = "#141314"
       , fgColor = "#a39b84"
       , position = TopP 0 109
       , lowerOnStart = True
       , commands = [
                      Run DynNetwork ["-L","1000","-H","5000","--normal","darkorange","--high","darkred"] 10
                    , Run Cpu ["-L","10","-H","50","--low","darkgreen","--normal","darkorange","--high","darkred"] 10
                    , Run Memory ["-t","<usedratio>%", "-L","30", "-H","75","--low", "darkgreen","--normal","darkorange", "--high","darkred"] 10
                    , Run Date "%a %b %_d %Y %H:%M" "date" 600
		    , Run Battery        [ "--template" , "Batt: <acstatus>"
			    , "--Low"      , "10"        -- units: %
			    , "--High"     , "80"        -- units: %
			    , "--low"      , "darkred"
			    , "--normal"   , "darkorange"
			    , "--high"     , "darkgreen"

			    , "--" -- battery specific options
			    -- discharging status
			    , "-o"	, "<left>% (<timeleft>)"
			    -- AC "on" status
			    , "-O"	, "<fc=#dAA520>Charging</fc>"
			    -- charged status
			    , "-i"	, "<fc=#006000>Charged</fc>"
			    ] 50
                    , Run Com "/data/bin/email.sh" [] "msgs" 300
                    , Run StdinReader
       ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }  <fc=darkorange>%date%</fc> | %msgs% { %dynnetwork% | %cpu% | Mem: %memory% | %battery%"
       }
