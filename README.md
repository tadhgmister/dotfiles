# dotfiles

This is my personal configuration for GNU guix system, it should contain all the necessary details to reconstruct my system
on a new machine with minimal effort. This isn't perfect in practice, I occasionally use "guix install" for something I want available for a while and then build a further dependency on it forgetting it isn't in any of my saved configurations
and there are often configurations for applications that don't make it into these configs such as installing/updating brave (I get it through nix) or browser extensions. 

I use alacritty as my terminal and dwm as my window manager, I specifically set up dwm to hide the top bar unless I'm holding the windows key and any time a window asks for attention it shows the top bar such that tapping the windows key hides it. alacritty sends this signal when it prints the bel character so by putting a bel character in my prompt message I get a noticable but non intrusive alert when a command finishes in another space. Similarly my chat client dino raises the same signal wen I get a new message so chat notifications are noticable but unintrusive to my work.

I am a fan of duckduckgo's bangs and have added a list of bangs I find useful to the list of auto-completes in dmenu so they open in brave directly. This lets me type windows+space "!wea" to autofill the whole command needed to visit environment canadas website for ottawa weather.

Both dino and libinput are using outdated versions, dino because when they updated to version 4 to implement "proper" notifications they removed the x signals that most other desktop environments ignore but I rely on for my notifications
and libinput because they had a bug where it broke secondary click on my framework trackpad and I submitted a single line patch to fix it, it got accepted and is probably in the main stream now but referencing the exact git version of my fix feels nice.

