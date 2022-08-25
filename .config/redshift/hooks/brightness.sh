#!/usr/bin/env sh

set -eu

exec >> ~/.redshift-hooks.log 2>&1

# path to github.com/qualiaa/acpi-brightness script
readonly brightness=$HOME/usr/bin/brightness 

readonly fade_time=$(( 60*60 ))
readonly day_brightness=90
readonly transition_brightness=60
readonly night_brightness=35


brightness() {
    local seconds=${1:-0}
    local steps=$((seconds * 100))
    xbacklight -time $((seconds*1000)) -set $percent
}

if [ $1 = "period-changed" ]; then
    case $3 in
        daytime)
            percent=$day_brightness
            case $2 in
                transition)
                    brightness $fade_time --inc ;;
                night|none)
                    brightness ;;
                *)
                    echo "Unrecognised: $2"
            esac ;;
        transition)
            percent=$transition_brightness
            case $2 in
                daytime)
                    brightness $fade_time --dec ;;
                night)
                    brightness $fade_time --inc ;;
                none)
                    brightness ;;
                *)
                    echo "Unrecognised: $2"
            esac ;;
        night)
            percent=$night_brightness
            case $2 in
                transition)
                    brightness $fade_time --dec ;;
                daytime|none)
                    brightness ;;
                *)
                    echo "Unrecognised: $2"
            esac
    esac
fi

