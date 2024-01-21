if [get_string "" {Enter String} {Sample string dialog} XXXX result] {
    error_notify "" "Entered $result" Info
}
