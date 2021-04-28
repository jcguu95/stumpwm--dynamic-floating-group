## TODOs

+ [X] make every function default at group = (current-group).
      make every function check if input group is a
      dyn-float-group.

+ [X] Free a window when it is controlled my mouse.

+ [X] Add a function to print the `dyn-order` (state) of the
      (current) group, in order to make the development easier.

+ [X] Add more commands and keybindings.

+ [X] `#'re-tile` should respect modeline and boarder.. or even
  gap in the future Waiting for the fix for a related issue for
  general floating group:
  https://github.com/stumpwm/stumpwm/issues/864

+ [X] Make layout independent in each group.
      
+ [ ] Add `#'fullscreen` for this group. When invoked, every
      thing should be full. When called again, everything should
      be tiled back to when it was.

## Contributions

+ [ ] While a window is removed from the group, correctly switch
      focus to the next window in `dyn-order`.

+ [ ] Master swapping / focusing.
 
      s-H, s-L : (un)swap with master
      s-h, s-l : (un)focus on master

+ [ ] When some window is free, permute (oppositely) does not
      work for the first un-free window in `dyn-order` because I
      force the freed windows to be the first elements in the
      list `dyn-order`. Fix this bug.
      
## Related issues

+ While moving a floating window from a floating group `g1` to
  another group `g2`, the window isn't removed immediately from
  `g1` until the user switch focus to `g2`. This is a separate
  issue, and has been reported
  [here](https://github.com/stumpwm/stumpwm/issues/879).

