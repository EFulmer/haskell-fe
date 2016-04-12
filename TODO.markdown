- Redo battling; battle data structure should be a stack of moves, and folding
  through it should yield a tuple of (Char1's XP gains, Char2's XP gains)
- Main menu driver with different options:
-- Create character.
-- Battle existing characters.
-- View characters.
- Standardize indentation to 2 spaces.
- *TESTS.*
- Derive my own Lens, Generic, and To/FromJSON instances (fun exercise, which is the entire point of this!)
- Maybe switch JSON for YAML later?
