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


class DFA:
    def __init__(self, combos):
        self.__max_len = max(len(c.actions) for c in combos)

        self.__actions = dict()
        for c in combos:
            for a in c.actions:
                if a not in self.__actions:
                    self.__actions[a] = len(self.__actions)

        states = {(): 0}
        for combo in combos:
            for i in range(1, len(combo.actions)):
                state = combo.actions[:i]
                if state not in states:
                    states[state] = len(states)

        self.__finishing_states = [
            [[] for _ in range(len(self.__actions))] for _ in range(len(states))
        ]
        for combo in combos:
            start = combo.actions[:-1]
            for state, state_idx in states.items():
                if state[len(state) - len(start) :] == start:
                    action_idx = self.__actions[combo.actions[-1]]
                    self.__finishing_states[state_idx][action_idx].append(combo)

        self.__transitions = [[0] * len(self.__actions) for _ in range(len(states))]
        for state in states:
            for action in self.__actions:
                suffix = (*state, action)
                while suffix:
                    if suffix in states:
                        self.__transitions[states[state]][self.__actions[action]] = (
                            states[suffix]
                        )
                        break
                    suffix = suffix[1:]

        self.__state = 0

    def advance(self, action):
        action_idx = self.__actions.get(action)
        if action_idx is None:
            self.__state = 0
            return []
        else:
            completed_combos = self.__finishing_states[self.__state][action_idx]
            self.__state = self.__transitions[self.__state][action_idx]
            return completed_combos

    @property
    def max_len(self):
        return self.__max_len


class Combo:
    def __init__(self, line, possible_actions):
        actions, self.name, self.fighter = line.split("/")
        self.actions = tuple(actions.split(","))
        # TODO: copy these checks in Haskell if we did not already
        assert len(self.actions) >= 1
        assert all(a != "" for a in self.actions)
        assert all(a in possible_actions for a in self.actions)

    def __repr__(self):
        return f"{self.name} ({self.fighter}) !!"


def parse_dfa(combo_lines, possible_actions):
    cache = set()
    combos = []
    for combo_line in combo_lines:
        combo = Combo(combo_line, possible_actions)
        identity = (combo.name, combo.fighter)
        assert identity not in cache, identity
        cache.add(identity)
        combos.append(combo)
    return DFA(combos)


def parse_keymap(keymap_lines):
    keymap = dict()
    for keymap_line in keymap_lines:
        key, action = keymap_line.split("/")
        key = key.upper()
        assert len(key) == 1 and key.isalpha() or key in ARROW_KEYS.values(), key
        assert key not in keymap, key
        keymap[key] = action
    return keymap


def lines(s):
    return [line.strip() for line in s.strip().split("\n") if line]


def parse_file(filename):
    keymap_str, combos_str = open(filename).read().strip().split("\n\n")
    keymap = parse_keymap(lines(keymap_str))
    dfa = parse_dfa(lines(combos_str), set(keymap.values()))
    return (keymap, dfa)


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
    keymap, dfa = parse_file(sys.argv[1])
    actions = deque([], maxlen=dfa.max_len)
    try:
        for action in gen_actions(keymap):
            actions.append(action)
            print(", ".join(actions))
            for combo in dfa.advance(action):
                print(combo)
            print()
    except KeyboardInterrupt:
        print("Goodbye.")


if __name__ == "__main__":
    main()
