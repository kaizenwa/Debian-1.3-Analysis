# FRENCH.PERL by Nikos Drakos <nikos@cbl.leeds.ac.uk> 25-11-93
# Computer Based Learning Unit, University of Leeds.
#
# Extension to LaTeX2HTML to translate LaTeX french special 
# commands to equivalent HTML commands and ISO-LATIN-1 characters.
# Based on a patch to LaTeX2HTML supplied by  Franz Vojik 
# <vojik@de.tu-muenchen.informatik>. 
#
# Change Log:
# ===========
#
# 11-MAR-94 Nikos Drakos - Added support for \inferieura and \superrieura

package french;

# Put french equivalents here for headings/dates/ etc when
# latex2html start supporting them ...

sub main'french_translation {
    @_[0];
}

package main;

sub do_cmd_frenchTeX {
    # Just in case we pass things to LaTeX
    $default_language = 'french';
    $latex_body .= "\\frenchTeX\n";
    @_[0];
}

sub do_cmd_originalTeX {
    # Just in case we pass things to LaTeX
    $default_language = 'original';
    $latex_body .= "\\originalTeX\n";
    @_[0];
}

sub do_cmd_inferieura {
   "&lt @_[0]"
}
 
sub do_cmd_superrieura {
   "&gt @_[0]"
}

#AYS: Prepare the french environment ...
sub french_titles {
    $toc_title = "Table des mati&egrave;res";
    $lof_title = "Liste des figures";
    $lot_title = "Liste des tableaux";
    $idx_title = "Index";
    $bib_title = "R&eacute;f&eacute;rences";
    $abs_title = "R&eacute;sum&eacute;";
    $pre_title = "Pr&eacute;face";
    $app_title = "Annexe";
    $info_title = "&Agrave;propos de ce document..."; 
    @Month = ('', 'janvier', 'f&eacute;vrier', 'mars', 'avril', 'mai',
              'juin', 'juillet', 'ao&ucirc;t', 'septembre', 'octobre',
              'novembre', 'd&eacute;cembre');
}

#AYS(JKR): Replace do_cmd_today (\today) with a nicer one, which is more
# similar to the original. 
sub do_cmd_today {
    local($today) = (`date "+%m:%d, 20%y"`);
    $today =~ s/(\d{1,2}):0?(\d{1,2}),/$2 $Month[$1]/o;
    $today =~ s/20([7|8|9]\d{1})/19$1/o;
    join('',$today,$_[0]);
}

# ... and use it.
&french_titles;
$default_language = 'french';
$TITLES_LANGUAGE = "french";

1;				# Not really necessary...



