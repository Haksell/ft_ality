from collections import deque
from contextlib import contextmanager
import os
import sys
import termios
import tty

ARROW_KEYS = {
    65: "UP",
    66: "DOWN",
    67: "RIGHT",
    68: "LEFT",
}


class Combo:
    def __init__(self, actions, name, fighter):
        self.__len = len(actions)
        self.__str = f"{name} ({fighter}) !!"
        self.__state = 0
        self.__dfa = [dict() for _ in range(len(actions))]
        unique_actions = list(set(actions))
        for i in range(len(actions)):
            for a in unique_actions:
                for j in range(i + 1, 0, -1):
                    if actions[i - j + 1 : i] + [a] == actions[:j]:
                        self.__dfa[i][a] = j
                        break

    def __len__(self):
        return self.__len

    def __str__(self):
        return self.__str

    def advance(self, action):
        self.__state = self.__dfa[self.__state].get(action, 0)
        if self.__state == self.__len:
            self.__state = 0
            return True
        else:
            return False


def parse_keymap(keymap_lines):
    keymap = dict()
    for keymap_line in keymap_lines:
        key, action = keymap_line.split("/")
        key = key.upper()
        assert len(key) == 1 and key.isalpha() or key in ARROW_KEYS.values(), key
        assert key not in keymap, key
        keymap[key] = action
    return keymap


def parse_combos(combo_lines):
    combos = []
    cache = set()
    for combo_line in combo_lines:
        actions, name, fighter = combo_line.split("/")
        identity = (name, fighter)
        assert identity not in cache, identity
        cache.add(identity)
        combos.append(Combo(actions.split(","), name, fighter))
    return combos


def lines(s):
    return [line.strip() for line in s.strip().split("\n") if line]


def parse_file(filename):
    keymap_str, combos_str = open(filename).read().strip().split("\n\n")
    return (
        parse_keymap(lines(keymap_str)),
        parse_combos(lines(combos_str)),
    )


@contextmanager
def cbreak_mode():
    old_settings = termios.tcgetattr(sys.stdin)
    try:
        tty.setcbreak(sys.stdin.fileno())
        yield
    finally:
        termios.tcsetattr(sys.stdin, termios.TCSADRAIN, old_settings)


def gen_keypresses():
    with cbreak_mode():
        while True:
            b = os.read(sys.stdin.fileno(), 3)
            if len(b) == 1 and b[0] < 127 and chr(b[0]).isalpha():
                yield chr(b[0]).upper()
            elif len(b) == 3 and b[0] == 27 and b[1] == 91:
                yield ARROW_KEYS.get(b[2])


def gen_actions(actions):
    for key in gen_keypresses():
        action = actions.get(key)
        if action is not None:
            yield action


def main():
    keymap, combos = parse_file(sys.argv[1])
    actions = deque([], maxlen=max(map(len, combos)))
    try:
        for action in gen_actions(keymap):
            actions.append(action)
            print(", ".join(actions))
            for combo in combos:
                if combo.advance(action):
                    print(combo)
            print()
    except KeyboardInterrupt:
        print("Goodbye.")


if __name__ == "__main__":
    main()
