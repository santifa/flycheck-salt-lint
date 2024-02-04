# flycheck-salt-lint

[Flycheck](https://www.flycheck.org/en/latest/) support for the [salt-lint](https://github.com/warpnet/salt-lint/) checker.

## Setup

Install salt-lint locally via pip:

``` shell
pip install salt-lint
```

### Manual installation

Download this file, put it into your load path and add the following to your `init.el`:

``` emacs-lisp
(require flycheck)
(require flycheck-salt-lint)
```

## License

Copyright (c) 2024 Henrik Jürges <ratzeputz@rtzptz.xyz>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
