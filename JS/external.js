/*
external.js
James Watson, 2015 January
Handle communication between REPLacement and the environment not handled by the evaluator: environment-dependent, file operations, etc
*/

/*
  == LOG ==
2015-02-27: 'External.read_file_in_load_path' now makes use of 'parse_and_eval_lines' instead of 'load_by_lines'
2015-02-23: 'External.read_file_in_load_path' now makes use of 'load_by_lines' instead of 'External.readln_from_current_file'
2015-02-04: 'External.choose_file_and_load' successfully reads, parses, and interprets a file in the '$global' context, consistently and on the first 
            attempt.  As expected, variables and functions defined in the file are available for use in the REPL, which is also interpreted in the 
            '$global' context.
2015-02-02: * 'External.read_file_in_load_path' does not seem to load the file consistently. It must be called twice in order to achieve the desired 
              result. Cause is unknown.
            * Abandoned: 'External.read_file_in_load_path', 'External.readln_from_current_file'
            * User will choose a file to load individually from the GUI via 'External.choose_file_and_load'. This has limitations, as the ability to
              programmatically load libraries is a powerful tool that supports the DRY Principle.
2015-01-31: Corrected index check in 'External.readln_from_current_file'
2015-01-29: Wrote 'External.readln_from_current_file', tested OK
2015-01-27: * 'External.load' is able to parse a file into lines
            * Call to 'getFile' in 'External.load' referred to an old 'External' structure, repaired
2015-01-26: 'External.load' is able to load a file by name from the 'External.curDirObjct'
2015-01-24: 'External.choose_file_load_path' now finds full path string
2015-01-23: It was necessary to add "directory" to the 'fileSystem' permissions in order to get the dialog box to open
2015-01-19: File created


  == NOTES ==
2015-02-11: URL, message passing: https://developer.chrome.com/extensions/messaging
2015-02-02: URL: https://developer.chrome.com/apps/fileSystem
2015-01-27: * 'FileReader' does not have a facility for reading a text file by lines, client code must divide the returned string as desired
            * URL: https://developer.mozilla.org/en-US/docs/Web/API/FileReader
2015-01-26: * At the moment, Chrome 'fileSystem' does not have a stock means to reach local directories other than a chooser dialog box. (This is likely
              for security reasons.) Once a 'DirectoryEntry' has been created, you can then use file name strings to reach the individual files 
              programmatically. In this case via a call to a load function within REPLacement.
            * URL: http://www.developer.com/lang/reading-and-writing-files-in-chrome-installed-applications.html
            * URL: www.html5rocks.com/en/tutorials/file/filesystem/
            * URL: https://developer.mozilla.org/en-US/docs/Web/API/DirectoryEntry
            * URL: https://developer.mozilla.org/en-US/docs/Web/API/FileEntry
2015-01-19: For the time being file saving will be left to the user's preferred source editor, only loading will be handled


  == TODO ==
* Try having 'External' send the parent App a message when it is done loading a file
* Each time such an object is constructed, it should be checked whether the file that is pointed to is readable/writeable
* Investigae Chrome synced storage.  Giving up the ability to programmatically load files so that one may be reliably loaded is a poor bargain.
*/

var External = {}; // The 'External' object is a singleton that holds functions and vars for external operations

// Store current working directory and file for use by 'External' methods
External.curPath = { // The current working directory, the root directory for all file load operations
	name : "", //- String, text representation of the current path
	objt : null // Object, 'DirectoryEntry' of the current path
};
External.curFile = { // File currently open
	name : "", // -- String,  Name of the file
	objt : null, //- Object,  'FileEntry' of the file
	lins : [], // -- Array,   Lines of the file, each element is a string
	lnnm : 0, // --- Number,  The line number being read from the file
	lsln : -1, // -- Number,  The last line that was read 
	lodd : false, // Boolean, Flag for whether the last load attempt was successful
	rEOF : false //- Boolean, Flag for whether the last line of the file has been read (not actual EOF character)
};

External.choose_file_load_path = function(){ // Open a dialog for a user to choose a load path and store path name

	chrome.fileSystem.chooseEntry(
		{ type: 'openDirectory' },
		function(dirEntry, entryProps){ 
			External.curPath.objt = dirEntry;
			chrome.fileSystem.getDisplayPath(
				dirEntry, 
				function(displayPath){ 
					External.curPath.name = displayPath;
					// Works in both Version 39.0.2171.95 and HOMECOMPVERSION
					preDebug.innerText = displayPath; // FIXME: Remove debug behavior
				} 
			);
			//External.read_file_in_load_path( "testFile.txt" );
		}
	);

};

External.choose_file_and_load = function(){ // Open a GUI dialog to choose a file, attempt to load, parse, and evaluate file

	chrome.fileSystem.chooseEntry(
		{ type: 'openFile' }, // Prompts the user to open an existing file and returns a FileEntry on success.
		function(filEntry, entryProps){ 
			filEntry.file( function(file){ // Returns 'File' object associated with selected file. Use this to read the file's content.
				var reader = new FileReader();
				var fileLines;
				/* reader.onload = function(e){
					External.curFile.lodd = true; // File load success
				}; */
				reader.onloadend = function(e){
					//var contents = e.target.result;
					// URL, split string into lines: http://stackoverflow.com/questions/12371970/read-text-file-using-filereader
					fileLines = e.target.result.split('\n'); // split the string result into individual lines
					load_by_lines( fileLines ); // Send the lines to REPLacement evaluator
				};
				reader.readAsText(file);
				//External.curFile.lnnm = 0; // Set current line to 0 for the newly-loaded file
				//External.curFile.rEOF = false; // Reset EOF flag
				
			} );
		}
	);

};

// == Experimentally Reinstated ==

External.read_file_in_load_path = function(nameStr, context){ // Read the lines of 'nameStr' into 'External.curFile.lins'
	External.curPath.objt.getFile( // call 'DirectoryEntry.getFile' to fetch a file in that directory 
		nameStr,
		{create: false},
		function(fileEntry){ // action to perform on the fetched file, success
			External.curFile.name = nameStr; // store the file name for later use
			External.curFile.objt = fileEntry; // store the 'FileEntry' for later use
			
			External.curFile.objt.file( function(file){ // Returns 'File' object associated with selected file. Use this to read the file's content.
				var reader = new FileReader();
				reader.onload = function(e){
					External.curFile.lodd = true; // File load success
				};
				reader.onloadend = function(e){
					//var contents = e.target.result;
					// URL, split string into lines: http://stackoverflow.com/questions/12371970/read-text-file-using-filereader
					External.curFile.lins = e.target.result.split('\n'); // split the string result into individual lines
					parse_and_eval_lines( External.curFile.lins );
					/*while(!External.curFile.rEOF){ // determine the 'meaning' of a file line by line
						curLineMeaning = meaning( s( External.readln_from_current_file() ), context); 
						preDebug.innerText += '\r\nNext Line?: ' + External.curFile.lnnm;
						preDebug.innerText += '\r\nEOF?: ' + External.curFile.rEOF;
					}*/
				};
				reader.readAsText(file);
				External.curFile.lnnm = 0; // Set current line to 0 for the newly-loaded file
				External.curFile.rEOF = false; // Reset EOF flag
				// let's try a message instead of a flag ...
				/*chrome.runtime.sendMessage({greeting: "hello"}, function(response) {
					console.log(response.farewell);
				});*/
			} );
			
		},
		function(e){ External.curFile.lodd = false; } // There was an error
	);
	// External.curFile.lodd = true; // File load success // HANGS IF HERE!?!
};

External.readln_from_current_file = function(){ // Return the next line of the current file, otherwise return 'null'
	var rtnStr = null; // Default error code
	if( External.curFile.lnnm < External.curFile.lins.length ){ // Check that the line number is valid
		rtnStr = External.curFile.lins[External.curFile.lnnm];
		External.curFile.lsln = External.curFile.lnnm; // Set the line number as the last line read
		External.curFile.lnnm = External.curFile.lnnm + 1; // advance line number to the next line
		if( External.curFile.lnnm >= External.curFile.lins.length ){ 
			External.curFile.rEOF = true; 
			preDebug.innerText += '\r\nEOF reached!';
		} // check, set EOF flag true
	} // else the error code will be returned below
	return rtnStr;
};

// == End Reinstated ==

/* == Abandoned Code ==

// == End Abandoned == 
*/
