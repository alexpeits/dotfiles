-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

-- This is setup for dual 1920x1080 monitors, with the right monitor as primary
Config {
	font = "xft:DejaVu Sans Mono for Powerline:size=10.2:antialias=true",
    borderColor = "black",
    border = TopB,
    bgColor = "black",
    fgColor = "grey",
    position = Static { xpos = 0, ypos = 0, width = 1800, height = 22 },
    commands = [
        Run Weather "CYVR" ["-t","<tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000,
        Run Network "enp0s31f6" ["-L","0","-H","32","--normal","green","--high","red"] 10,
        Run Network "wlp3s0" ["-L","0","-H","32","--normal","green","--high","red"] 10,
        Run Cpu ["-t", "cpu: <total>%", "-L","3","-H","50","--normal","green","--high","red"] 10,
        Run Memory ["-t","mem: <usedratio>%"] 10,
        Run Swap [] 10,
        Run Com "uname" ["-s","-r"] "" 36000,
        Run Date "%a %b %_d %Y %H:%M:%S" "date" 10,
        Run Brightness ["--template", "bl: <percent>", "-D", "intel_backlight"] 10,
        Run Com "/bin/bash" ["-c", "~/.xmonad/get-volume.sh"] "myvolume" 1,
        Run Com "/bin/bash" ["-c", "~/bin/getlayout.sh"] "mylayout" 1,
        Run Battery [
            "-t", "<acstatus> <left>%",
            "--Low", "20",
            "--High", "80",
            "--low", "red",
            "--normal", "orange",
            "--high", "green",
            "--",
            --"-c", "charge_full",
            "-f", "ACAD/online",
            "-O", "⚡",
            "-o", "▅",
            "-h", "green",
            "-l", "red"
        ] 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader%}{ %cpu% | %memory% | %wlp3s0% | %enp0s31f6% | %mylayout% | <fc=#ee9a00>%date%</fc> | ♫ %myvolume% | %battery% ",
    lowerOnStart = False
}
