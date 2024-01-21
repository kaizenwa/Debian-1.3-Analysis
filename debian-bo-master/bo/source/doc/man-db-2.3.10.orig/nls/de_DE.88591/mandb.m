$set 14 #mandb

$ #_USAGE Original Message:(usage: %s [-dqsuc|-h|-V] [manpath]\n)
# usage: %s [-dqsuc|-h|-V] [manpath]\n

$ #_OPTIONS Original Message:(-d --debug                  produce debugging info.\n\
-q --quiet                  work quietly, except for 'bogus' warning.\n\
-s --no-straycats           don't look for or add stray cats to the dbs.\n\
-u --user-db                produce user databases only.\n\
-c --create                 create dbs from scratch, rather than updating.\n\
-V --version                show version.\n\
-h --help                   show this usage message.\n)
# -d --debug                  Erzeuge Debugging-Ausgaben.\n\
-q --quiet                  Arbeite ohne Ausgabe, bis auf seltsame Fehler\n\
                            meldungen.\n\
-s --no-straycats           Füge Manual-Seiten ohne Quellcode nicht in die\n\
                            Datenbanken ein.\n\
-u --user-db                Erzeuge nur Benutzer-Datenbanken.\n\
-c --create                 Erzeuge Datenbanken völlig neu.\n\
-V --version                Zeige Versionnummer an.\n\
-h --help                   Zeige diese Options-Zusammenfassung an.\n

$ #_REMOVE Original Message:(can't remove %s)
# Kann %s nicht löschen

$ #_RENAME Original Message:(can't rename %s to %s)
# Kann %s nicht in %s umbenennen

$ #_CHMOD Original Message:(can't chmod %s)
# Kann Besitzer und/oder Gruppe von %s nicht ändern

$ #_CHOWN Original Message:(can't chown %s)
# Kann Zugriffsrechte von %s nicht ändern

$ #_PROCESS Original Message:(Processing manual pages under %s...\n)
# Bearbeite Manual-Seiten unter %s...\n

$ #_NO_USER Original Message:""(the setuid man user %s does not exist)
# Der von Man benutze User %s existiert nicht

$ #_NO_DIRECTIVES Original Message:(warning: no MANDB_MAP directives in %s, using your manpath)
# Warnung: Keine MANDB_MAP Anweisung in %s, benutze den Manualpfad (MANPATH)

$ #_MANS Original Message:(%d man subdirectories contained newer manual pages.\n\
%d manual pages )
# %d Manual-Verzeichnisse enthielten neuere Manual-Seiten.\n\
%d Manual-Seiten 

$ #_STRAYS Original Message:(and %d stray cats )
# und %d Manual-Seiten ohne Quellcode 

$ #_ADDED Original Message:(were added.)
# wurden hinzugefügt.
