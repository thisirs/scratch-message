# scratch-message.el

This utility allows you to automatically insert messages in your
scratch buffer coming from various sources.

## Installation

Just put the following in your `.emacs`:

```lisp
(require 'scratch-message)
```

## Configuration

You can customize `scratch-message-function` that is called to insert
a message. This function should use `scratch-message-insert` that is
responsible for the actual inserting.
