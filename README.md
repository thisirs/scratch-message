# scratch-message

This package allows you to automatically insert preconfigured messages
in your scratch buffer coming from various sources.

## Installation

Just put the following in your `.emacs`:
```lisp
(require 'scratch-message)
```

## Configuration

By default, a collection of famous quotes stored in
`scratch-message-quotes` are displayed. More generally, you can
customize the variable `scratch-message-function` that is called to
insert or change a quote. This function should call
`scratch-message-insert` that is responsible for the actual inserting.
