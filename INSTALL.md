# Downloading ToDo List
- If using Windows, make sure the `finalproject` repository is in your WSL files system.
You can do this by saving the repoository into the directory you want to work from, if it's Downloads folder then running the following command. 
`$ cp /mnt/c/Users/<your Windows user name>/Downloads/a1-release.zip .`
- If using Mac, just save the repository in which ever directory you wish to work from.

# Install Graphics:
Please run the following in your command line:
- `opam install graphics`
- `opam install user-setup`
- `opam user-setup install`

# Install Yojson:
Please run the following in your command line:
- `opam install yojson`

# Install ANSITerminal
Please run the following commands in your command line: 
- `opam update`
- `opam upgrade`
If you get an output in the form of "Everything as up-to-date as possible [...] Nothing to do", run `opam install ANSITerminal`

# Run ToDo List
Please run the following commands in the command line:
- `dune build` 
- `make todo`
In order to add a todo item:
- `add [priority] [item]`
Priority is optional and can be red, yellow, or green. If no priority is given, green is the default.
In order to delete a todo item:
- `delete [id]`
The id is the number to the left of the todo item
In order to change the priority of an item:
- `change [id] [priority]`
In order to add a note:
- `add note [note]`

