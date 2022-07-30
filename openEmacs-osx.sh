# - Use automator to create this application.
# - Can bind to a shortcut by creating a second automator
#   "Quick Application" and setting it to run this script.
#    - Then go to keyboard settings and services
# shortcuts and run
# - Note: Must be backgrounded, otherwise error results when exiting from capture
#   prompt (before note is added/captured on the next screen)
/opt/homebrew/bin/emacsclient -e "(progn (select-frame-set-input-focus (selected-frame)) (org-capture))" &
