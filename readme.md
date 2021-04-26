# StumpWM - Dynamic Floating Group

Dynamic tiling + floating support for StumpWM!

## Demo

+ Basic Movement

  ![Basic Movement](img/basic-movement.gif)

+ Float and Retile

  ![Float and Retile](img/float-and-retile.gif)

+ Layout and Ratio

  ![Layout and Ratio](img/layout-and-ratio.gif)

## Usage

I will try to merge it to the main StumpWM repo after enough
testing. In the meanwhile, connect StumpWM to `swank` or `slynk`,
and evaluate the file in the repl.

## TODOs

+ [X] make every function default at group = (current-group).
      make every function check if input group is a
      dyn-float-group.

+ [X] Free a window when it is controlled my mouse.

+ [X] Add a function to print the dyn-order (state) of the
      (current) group, in order to make the development easier.

+ [X] Add more commands and keybindings.

+ [X] `#'re-tile` should respect modeline and boarder.. or even
  gap in the future Waiting for the fix for a related issue for
  general floating group:
  https://github.com/stumpwm/stumpwm/issues/864

+ [ ] Emacs drop-down window.

  + See [`equake`](https://babbagefiles.xyz/equake-elisp-console/).

  + Invoke an emacs window with customized frame.

  ```
  $ emacsclient -c -F '((name . "floating") (width . 150) (height . 10))' \
                -e '(ivy-read "hi" (list (quote a)))'
  ```

+ [ ] Found a bug.. when some window is floating, permute might
      not work. I should separate free windows and unfree windows
      into two different lists. This is harder to fix cuz it will
      change the infrastructure a bit.

+ [ ] Master swapping / focsing.
 
      s-H, s-L : (un)swap with master
      s-h, s-l : (un)focus on master

+ [ ] Add a #'fullscreen for this group. When invoked, every
      thing should be full. When called again, everything should
      be tiled back to when it was.

+ [ ] Another general issue is that the floating windows remain
      as a floating-window instance while "thrown" to a
      tiling-group. This should not happen. (?)
      
+ [ ] Make layout independent in each group.

## Related issues

+ While moving a floating window from a floating group `g1` to another group
  `g2`, the window isn't removed immediately from `g1` until the user switch
  focus to `g2`. This is a separate issue, and has been reported
  [here](https://github.com/stumpwm/stumpwm/issues/879).
