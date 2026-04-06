# Contributing

If you discover issues, have ideas for improvements or new features, please
report them to the [issue tracker][1] of the repository or submit a pull
request. Please, try to follow these guidelines when you do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `main`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Include a screenshot illustrating the issue.

## Pull requests

* Read the [design principles][2] to understand the color semantics and
  conventions used throughout the theme.
* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][3].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Add missing face for ...`).
* Update the [changelog][4].
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (`C-c ? d`).
* Add a before/after screenshot illustrating your changes visually.
* [Squash related commits together][5].
* Open a [pull request][6] that relates to *only* one subject with a clear title
  and description in grammatically correct, complete sentences.

[1]: https://github.com/bbatsov/tokyo-night-emacs/issues
[2]: https://github.com/bbatsov/tokyo-night-emacs/blob/main/DESIGN.md
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://github.com/bbatsov/tokyo-night-emacs/blob/main/CHANGELOG.md
[5]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
[6]: https://help.github.com/articles/using-pull-requests
