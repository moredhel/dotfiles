-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

-- This is setup for dual 1920x1080 monitors, with the right monitor as primary
-- This setup has been chonged to work an a laptop
Config {
    font = "xft:Fixed-5",
    bgColor = "#000110",
    fgColor = "#fffaaa",
    position = Static { xpos = 0, ypos = 3, width = 1024, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Weather "EGPH" ["-t","<tempC>C <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000,
        -- Run MultiCpu ["-t","Cpu: <total0> <total1> <total2> <total3>","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        -- Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Network "wlp2s0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Date "%a %b %_d %l:%M" "date" 10,
	Run Kbd [("us(dvorak)", "DV"), ("us", "US")],
	Run BatteryP ["BAT1"] ["-t", "<left>%","-H","65","--high","green","-L","25","-l","red"] 10,
	Run Com "mpc" ["-f", "\"%artist% - %title% - %time%\""] "mpd" 1000,
        Run StdinReader

    ],
    sepChar = "%",
    alignSep = "}{",
    template = "<fc=#00DD00>%kbd%</fc> - %StdinReader%}{<fc=#aFFFCC>| %date%</fc> | %EGPH%  %wlp2s0%  %battery%"
}
