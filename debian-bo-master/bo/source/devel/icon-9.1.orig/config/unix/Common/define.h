/*
 * Most definitions used to configure Icon source code have defaults
 * that only need to be overriden in specific cases. The defaults are
 * listed in the installation manual (TR 90-2).
 *
 * In cases where the defaults are not appropriate, alternative
 * definitions should be given here.
 *
 * The following definitions get things off the ground. The first
 * one needs changing. The last two need to be removed if co-expressions
 * and arithmetic overflow checking are implemented.
 *
 * This comment itself should be removed when the installation is
 * complete.
 */

#define HostStr "unknown host"
#define NoCoexpr

/*
 * Do not remove the following definition. It controls many
 * aspects of conditional assembly that are specific to UNIX
 * systems.
 */

#define UNIX 1
