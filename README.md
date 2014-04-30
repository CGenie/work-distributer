# work-distributer

Distributer for files to edit, written in Haskell.

Add `~/.work-distributer` file with content like this one:

```
[directories]
source = ~/source-files
target = ~/target-files
```

Source files will be served, when work is done a new file can be uploaded
and it will hit the target directory. Source files are not deleted, but removed
from queue which is persistent as long as Acid State data is there.


## TODO:
* some error checking (for example, we use `head` sometimes and list length
  is not checked)
