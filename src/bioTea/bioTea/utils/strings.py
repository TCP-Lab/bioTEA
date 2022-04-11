import re
from itertools import zip_longest

from colorama import Fore

# 7-bit C1 ANSI sequences
ansi_escape = re.compile(
    r"""
    \x1B  # ESC
    (?:   # 7-bit C1 Fe (except CSI)
        [@-Z\\-_]
    |     # or [ for CSI, followed by a control sequence
        \[
        [0-?]*  # Parameter bytes
        [ -/]*  # Intermediate bytes
        [@-~]   # Final byte
    )
""",
    re.VERBOSE,
)


def strip_colors(string):
    return ansi_escape.sub("", string)


def combine_linewise(a, b, padding="", strip=False, align_bottom=False, fix_len=True):
    lines_a = a.split("\n")
    lines_b = b.split("\n")

    if align_bottom:
        lines_a = list(reversed(lines_a))
        lines_b = list(reversed(lines_b))

    def max_len(lines):
        return max([len(strip_colors(x)) for x in lines])

    if max_len(lines_a) > max_len(lines_b) and fix_len:
        fill = " " * max_len(lines_a)
    elif max_len(lines_b) > max_len(lines_a) and fix_len:
        fill = " " * max_len(lines_b)
    else:
        fill = ""

    result = []
    for line_a, line_b in zip_longest(lines_a, lines_b, fillvalue=fill):
        if strip:
            line_a, line_b = line_a.strip(), line_b.strip()
        line_a = line_a + padding
        result.append(line_a + line_b)

    if align_bottom:
        result = reversed(result)

    return "\n".join(result)


def make_square(logo, side="left"):
    assert side in ["left", "right"]
    lines = logo.split("\n")
    longest = max([len(strip_colors(x)) for x in lines])

    res = []
    for line in lines:
        padding = " " * (longest - len(strip_colors(line)))

        if side == "left":
            res.append(line + padding)
        elif side == "right":
            res.append(padding + line)

    return "\n".join(res)


TEA = """                                                    __/\__
             ;,'                               . _  \\\\''//
     _o_    ;:;' __    _     _______________   -( )-/_||_\\
 ,-.'---`.__ ;  / /_  (_)___/_  __/ ____/   |   .'. \_()_/
((j`=====',-'  / __ \/ / __ \/ / / __/ / /| |    |   | . \\
 `-\     /    / /_/ / / /_/ / / / /___/ ___ |    ϕ---| .  \\
    `-=-'    /_.___/_/\____/_/ /_____/_/  |_|   .'. ,\_____'.
                              W I Z A R D
"""

TEAPOT = (Fore.RESET + "\n").join(
    [
        Fore.LIGHTBLACK_EX + "             ;,'",
        Fore.LIGHTRED_EX + "     _o_    " + Fore.LIGHTBLACK_EX + ";:;' ",
        Fore.LIGHTRED_EX + " ,-.'---`.__ " + Fore.LIGHTBLACK_EX + ";  ",
        Fore.LIGHTRED_EX + "((j`=====',-'  ",
        Fore.LIGHTRED_EX + " `-\     /    ",
        Fore.LIGHTRED_EX + "    `-=-'    ",
    ]
) + Fore.RESET

BIOTEA = (Fore.RESET + "\n").join(
    [
        "    __    _     " + Fore.LIGHTGREEN_EX + "_______________ ",
        "   / /_  (_)___" + Fore.LIGHTGREEN_EX + "/_  __/ ____/   |",
        "  / __ \/ / __ \\" + Fore.LIGHTGREEN_EX + "/ / / __/ / /| |",
        " / /_/ / / /_/ " + Fore.LIGHTGREEN_EX + "/ / / /___/ ___ |",
        "/_.___/_/\____" + Fore.LIGHTGREEN_EX + "/_/ /_____/_/  |_|",
    ]
)

BIOTEA_S = (Fore.RESET + "\n").join(
    [
        "__    _     " + Fore.LIGHTGREEN_EX + "_______________ ",
        "/ /_  (_)___" + Fore.LIGHTGREEN_EX + "/_  __/ ____/   |",
        "/ __ \/ / __ \\" + Fore.LIGHTGREEN_EX + "/ / / __/ / /| |",
        "/ /_/ / / /_/ " + Fore.LIGHTGREEN_EX + "/ / / /___/ ___ |",
        "/_.___/_/\____" + Fore.LIGHTGREEN_EX + "/_/ /_____/_/  |_|",
    ]
) + Fore.RESET

WIZARD = (Fore.RESET + "\n").join(
    [
        Fore.LIGHTYELLOW_EX + "     __/\__",
        Fore.LIGHTCYAN_EX + ". _ " + Fore.LIGHTYELLOW_EX + " \\\\''//",
        Fore.LIGHTCYAN_EX + "-( )-" + Fore.LIGHTBLACK_EX + "/_||_\\",
        Fore.LIGHTCYAN_EX + " .'. " + Fore.LIGHTBLACK_EX + "\_()_/",
        "  |   " + Fore.LIGHTRED_EX + "| . \\",
        Fore.LIGHTBLACK_EX + "  ϕ" + Fore.LIGHTRED_EX + "---| .  \\",
        " .'. " + Fore.LIGHTRED_EX + ",\_____'.",
    ]
) + Fore.RESET

WIZARD_WORD = Fore.LIGHTCYAN_EX + "                          W I Z A R D" + Fore.RESET

TEA_LOGO = combine_linewise(
    TEAPOT, BIOTEA_S, strip=False, align_bottom=True, fix_len=False
)
WIZARD_LOGO = (
    combine_linewise(
        make_square(TEA_LOGO), WIZARD, strip=False, align_bottom=True, padding=" "
    )
    + "\n"
    + WIZARD_WORD
)
