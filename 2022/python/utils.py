from dataclasses import dataclass
import re


@dataclass
class RegexMatch(str):
    """
    Regular expression pattern matching helper class

    This class is used to be able to combine regular expression
    in the pattern matching functionality of Python (requires Python >= 3.10).
    """
    string: str
    match: re.Match = None

    def __eq__(self, pattern: str) -> bool:
        """
        Equal comparison function.

        :param pattern: Regular expression
        :return: Boolean indicating whether match has been found.   
        """
        self.match = re.search(pattern, self.string)
        return self.match is not None

    def __getitem__(self, group):
        """
        Get item of match results.

        :param group: Index of re.match function output, first output is the complete text,
                      so usually index of groups starts with 1 instead.
        :return: Matched string based on index number.
        """
        return self.match[group]
