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
    position = Static { xpos = 0, ypos = 0, width = 1770, height = 22 },
    commands = [
        Run Network "enp0s31f6" ["-L","0","-H","32","--normal","green","--high","red"] 30,
        Run Network "wlp3s0" ["-L","0","-H","32","--normal","green","--high","red"] 30,
        Run Cpu ["-t", "cpu: <total>%", "-L","3","-H","50","--normal","green","--high","red"] 30,
        Run Memory ["-t","mem: <usedratio>%"] 30,
        Run Date "%a %b %_d %Y %H:%M:%S" "date" 10,
        Run Com "/bin/bash" ["-c", "~/.xmonad/get-volume.sh"] "myvolume" 5,
        Run Com "/bin/bash" ["-c", "~/bin/getlayout.sh"] "mylayout" 5,
        Run Brightness ["--template", "☀ <percent>%", "--", "-D", "intel_backlight"] 5,
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
    template = "%StdinReader%}{ %cpu% | %memory% | %wlp3s0% | %enp0s31f6% | %bright% | ♫ %myvolume% | %battery% | %mylayout% | <action=`gsimplecal` button=1><fc=#ee9a00>%date%</fc></action> ",
    lowerOnStart = False
}
