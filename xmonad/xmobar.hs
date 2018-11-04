-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

-- This is setup for dual 1920x1080 monitors, with the right monitor as primary
Config {
    font = "xft:DejaVu Sans Mono for Powerline:size=10.2:antialias=true",
    borderColor = "black",
    border = TopB,
    bgColor = "#0c1014",
    fgColor = "#a9bfbe",
    position = Static { xpos = 0, ypos = 0, width = 1770, height = 22 },
    commands = [
        -- Run Network "enp0s31f6" ["-L","0","-H","32","--normal","#2bc9a2","--high","#e44439"] 30,
        -- Run Network "wlp3s0" ["-L","0","-H","32","--normal","#2bc9a2","--high","#e44439"] 30,
        Run Cpu ["-t", "cpu: <total>%", "-L","3","-H","50","--normal","#2bc9a2","--high","#e44439"] 30,
        Run Memory ["-t","mem: <usedratio>%"] 30,
        Run Date "%a %b %d %H:%M" "date" 10,
        Run Com "/bin/bash" ["-c", "~/bin/get-volume.sh"] "myvolume" 5,
        Run Com "/bin/bash" ["-c", "~/bin/getlayout.sh"] "mylayout" 5,
        Run Com "/bin/bash" ["-c", "~/bin/song-info.sh"] "mymusic" 5,
        Run Brightness ["--template", "☀ <percent>%", "--", "-D", "intel_backlight"] 5,
        Run Battery [
            "-t", "<acstatus> <left>%",
            "--Low", "20",
            "--High", "80",
            "--low", "#e44439",
            "--normal", "#dc7747",
            "--high", "#2bc9a2",
            "--",
            --"-c", "charge_full",
            "-f", "ACAD/online",
            "-O", "⚡",
            "-o", "▅",
            "-h", "#2bc9a2",
            "-l", "#e44439"
        ] 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader%}{ %cpu% | %memory% | %bright% | ♫ %myvolume%<fc=#728d8c>%mymusic%</fc> | %battery% | %mylayout% | <action=`gsimplecal` button=1><fc=#2daad0>%date%</fc></action> ",
    lowerOnStart = True
}
