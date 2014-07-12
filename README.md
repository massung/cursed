# The CURSED Package

The `cursed` package adds a subclass of the [output-pane](http://www.lispworks.com/documentation/lw61/CAPRM/html/capiref-275.htm#marker-4173290) element in [LispWorks'](http://www.lispworks.com) [CAPI](http://www.lispworks.com/documentation/lw61/CAPUG-M/html/capiuser-m.htm). It's designed to allow for simple, console-style display of text, but within a graphical pane.

## Quickstart

You can immediately start using the `cursed-pane` by just containing one.

	CL-USER > (setf c (capi:contain (make-instance 'cursed-pane)))
	#<CURSED-PANE  21C83D3B>
	
![Initial pane with cursor visible](https://raw.github.com/massung/cursed/master/screenshots/cursed-pane-01.png)
	
Once the pane is up, you can use all of the Common Lisp printing and formatting functions to render text to the pane (it is a character output stream also!).

	CL-USER > (princ "Hello, world!" c)
	"Hello, world!"
	
	CL-USER > (force-output c)
	NIL

![With obligatory example](https://raw.github.com/massung/cursed/master/screenshots/cursed-pane-02.png)

You can use the `cursed-pane-cursor-x` and `cursed-pane-cursor-y` accessor methods to `setf` the cursor position. You can also use the Common Lisp `file-position` function.

	CL-USER > (file-position c)
	(13 0)
	
	CL-USER > (file-position c '(20 10))
	
	CL-USER > (format c "[~{~a~^, ~}]" '(this is a test))
	NIL
	
	CL-USER > (force-output c)
	NIL

![Rendering text elsewhere...](https://raw.github.com/massung/cursed/master/screenshots/cursed-pane-03.png)

The cursor can also be toggled on and off with the `cursed-pane-cursor-visible-p` accessor.

You can use the `with-output-to-cursed-pane` macro to quickly position the cursor, render, and force output. The macro will also allow you to temporarily change the foreground and background colors of the pane as well.

	CL-USER > (with-output-to-cursed-pane (c :x 0 :y 20 :foreground :yellow :background :blue)
	            (loop for i from 1 to 10 do (print (* i i))))
	NIL
	
![Scrolling](https://raw.github.com/massung/cursed/master/screenshots/cursed-pane-04.png)

The above example shows that the cursed pane will also scroll. There is no history, however. You can force the pane to scroll a single line using `cursed-pane-scroll`.

It is also possible to completely clear the pane with `cursed-pane-clear`. This will also re-position the cursor back to <0,0>.

It is possible to use the mouse when `cursed-pane-selection-visible-p` is set the `T` to select regions of the pane. The selection can also be inspected and adjusted with `cursed-pane-selection-start` and `cursed-pane-selection-end` (both are `setf`-able). These are set similarly to `file-position`.

![Selection](https://raw.github.com/massung/cursed/master/screenshots/cursed-pane-05.png)

The selection can be copied to the clipboard with `cursed-pane-copy`. Note, however, that newlines are not tracked so a block of text is what will be in the clipboard.

Text from the clipboard can be pasted into the pane with `cursed-pane-paste`.