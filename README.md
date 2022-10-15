# Spreadsheet

Alpha software.

## Dependency

Leiningen dependency:

```clojure
[com.phronemophobic/spreadsheet "0.1.1"]
```

deps.edn dependency:

```clojure
com.phronemophobic/spreadsheet {:mvn/version "0.1.1"}
```

## Usage

### Try it!

```sh
clj -Sdeps {:deps\ {com.phronemophobic/spreadsheet\ {:mvn/version\ \"0.1.1\"}}} -M -m com.phronemophobic.membrane.spreadsheet
```

### Programmatic usage

Open a spreadsheet window. Expressions will be evaluated in the caller's namespace.

```clojure
(require '[com.phronemophobic.membrane.spreadsheet :as ss])

(ss/run)
```

## "Tutorial"

This isn't a real tutorial, but just enough info to do _something_ with the tool.

The toolbar buttons:  
`+` : add a new row of name, form, and value.  
`+42`: add a new row of name, number slider, and value.  
`+[]`: add a new row of name, ui editor, and value. The UI editor behavior is WIP and undocumented. Good luck!  
`+:)`: add a new row of name, custom component, and value. The first field is the name of a defui component and the second field is the initial value. Try `membrane.basic-components/counter` and `{:num 42}` and hit the `reset` button.  
`+!!`: adds a new row of name, form, and value. Most fields will eval when you type. This field will only reevalute when you hit the `go` button. Useful for side effects, long computations, or `require`s.  


The `edit` button let's you switch the namespace the forms are evaluated in. The `save` and `load` buttons are intended for saving/loading documents from a database, but are currently disabled.

## License

Copyright © 2021 Adrian

Distributed under the Eclipse Public License version 1.0.
