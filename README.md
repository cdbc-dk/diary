
# Daily Diary

## Project Overview

The Daily Diary project is a desktop application designed to allow users to create, edit, and browse daily journal entries. Each entry is associated with a specific date and can store textual content. The application provides a hierarchical view of entries based on year, week, and date, as well as a free-text search capability. Data is persisted locally in an SQLite database file (`.db3`). The project utilizes a layered architecture, separating the user interface, business logic (Business Object Model - BOM), and data persistence layers.

## Key Features

*   **Chronological Organization:** Diary entries are organized by Year, Week, and Date in a tree-like structure in the navigation panel (`trvNav`).
*   **Entry Management (CRUD):** Provides functionality to Add new entries, Edit existing entries, and Delete entries via dedicated buttons and keyboard shortcuts, managed through the `TDDCollection` (BOM).
*   **Text Content:** Each diary entry (`TDDCollectionItem`) can store a block of textual information (`fText: TStream`), loaded and displayed in the main memo area (`memText`).
*   **Data Persistence:** All diary entries are stored securely in a local SQLite database file (`.db3`), handled by the `bom/bom_dd.pas` unit interacting with a `TLiteDb` component and SQL commands from `global/daily_diary_const.pas`.
*   **Configuration:** Application settings (`TDDSettings`), including database location, backup options, and search preferences, are managed through a configurable INI file (`daily_diary.ini`).
*   **Autobackup:** Includes an optional feature for automatic backups of the primary database file at user-defined intervals (daily, weekly, or monthly), implemented in `bom/bom_dd.pas`.
*   **Text Search:** Implements a full-text search capability (`TddTextSearch`) to find diary entries based on keywords present in the text content or the entry's date string. Search behavior (case sensitivity, match all) is controllable via application settings. Operates on the `TDDCollection` via the `IBom_dd` interface.
*   **Observer Pattern:** Utilizes the observer pattern (`bc_observer`) to decouple the UI (`lfm_dataaware.pas`) from changes in the data managed by the BOM (`bom/bom_dd.pas`).

## Dependencies

To build and run the Daily Diary project from source, you will need:

*   **Free Pascal Compiler (FPC)**: A compatible version with your Lazarus IDE.
*   **Lazarus IDE**: Required for opening and compiling the `.lpr` project file and `.lfm` form files.
*   **External Utility Units**: The project relies on a set of custom utility units, likely part of a larger library developed by the author. These include units such as `bc_utilities`, `bc_datetime`, `bc_mtlinklist`, `bc_litedb`, `bc_observer`, `bc_memdataset`, `bc_msgqueue`, `bc_bomintf`, `bc_advstring`, `bc_textsearch`, `bc_trvhelp`, and `bc_pcthelp`. The source code for these units is **not included** in the provided files and is required to successfully compile the project. Specifically, the project requires `bc_litedb` for database access and `bc_textsearch` for the search functionality. **Without these external units, compilation from the provided source code will not be possible.**

## Building and Running (From Source)

Assuming you have FPC, Lazarus, and the required external utility units set up and accessible (e.g., by adding their source directory to the project's search path in Lazarus Project Options):

1.  Open the `daily_diary.lpr` project file in the Lazarus IDE.
2.  Lazarus should load the project and its associated form (`lfm_dataaware.pas`).
3.  Ensure that the external `bc_` units are accessible to the compiler (e.g., by adding their source directory to the project's search path in Project Options).
4.  Compile the project (`Run -> Compile`). This will generate an executable file (`daily_diary` or `daily_diary.exe`).
5.  Run the executable. Upon first run, it will create the necessary database file (`daily_diary.db3` by default, located in a `db` sub-directory relative to the executable, as defined by `DD_Databasename` in `daily_diary_const.pas`) and an INI settings file (`daily_diary.ini`).

## Getting Started (Compiled Application)

*(**Important Note:** The provided source code snippets demonstrate the application's logic and structure but do not include the necessary external libraries nor the Lazarus `.lfm` or `.res` files required for compilation. To compile and run the application from source, you would need the Lazarus IDE and these external dependencies. **This documentation describes the compiled application's behavior.**)*

Assuming you have a compiled executable and the necessary runtime files:

1.  Obtain the `daily_diary` executable and all associated dependency files.
2.  Place all application files in a dedicated directory on your system.
3.  Execute the `daily_diary` program.
4.  Upon the first launch, the application will create a database file (`daily_diary.db3`) and a configuration file (`daily_diary.ini`). By default, the database is created in a `db` subdirectory relative to the executable's location, based on the path defined in the `DD_Databasename` function within `global/daily_diary_const.pas`. The INI file (`daily_diary.ini`) is created in the same directory as the executable (`DD_Inifilename`).

## How to Use the Application

The main application window provides a straightforward interface for managing your diary entries. It features a navigation TreeView on the left side and a large text Memo area on the right where you read and write your entries.

### Main Interface Overview

*   **Navigation TreeView (Left):** Located on the left side of the window, this panel organizes your diary entries. The structure is hierarchical:
    *   Level 0: The Root node (usually labeled "Dates").
    *   Level 1: Nodes representing the Year of the entry (e.g., "2023").
    *   Level 2: Nodes representing the ISO Week Number within that year (e.g., "45").
    *   Level 3: Nodes representing the specific Date of the entry (e.g., "11/15/2022").
*   **Text Memo Area (Right):** This large area on the right displays the content of the currently selected diary entry. You will type your entries here when adding or editing.
*   **Top Panel Buttons:** A row of buttons at the top provides quick access to common actions like adding, deleting, updating, reading data, searching, and accessing settings.
*   **Status Bar (Bottom):** Displays information about the current mode (Browse, Edit, Insert, Search) and other status messages.

### Navigating and Reading Entries

To view the content of a diary entry:

1.  **Locate the entry:** In the Navigation TreeView on the left, expand the Year and Week nodes until you see the specific Date node you wish to read.
2.  **Select the Date node:** Click on the Date node (Level 3) corresponding to the entry.
3.  **Read the content:** The text content of that diary entry will automatically load and be displayed in the large Text Memo Area on the right.

### Adding a New Entry

You can add an entry for the current date (today).

1.  **Initiate Add:** Click the `+` button in the top panel.
2.  **Application Check:**
    *   If a diary entry for today already exists, the application will load and select that entry in the TreeView, automatically switching the form into `Edit` mode for that existing entry.
    *   If no entry exists for today, a new, blank entry is created internally with today's date assigned, and the form switches to `Insert` mode. The text memo area will clear, ready for your input.
3.  **Enter your content:** Type or paste your diary entry into the empty Text Memo Area.
4.  **Save the entry:** Click the `Save` button (which appears/becomes enabled in `Insert` or `Edit` mode) in the top panel, or press `Ctrl + S` when the text memo has focus.

**Example: Adding Your First Diary Entry**

Let's imagine this is the very first time you're using the Daily Diary application and you want to add your first entry for today's date.

1.  Launch the `daily_diary` application.
2.  Click the `+` button in the top panel. Since no entry exists for today (or any date), the application will switch to `Insert` mode. The Text Memo area will become active and empty.
3.  You'll see the caption of the text area change to something like "New [Today's Date]".
4.  Type your first diary entry, for example: "Today I started using my new Daily Diary app! Seems simple and effective."
5.  Click the `Save` button in the top panel. The application will process the change, add the entry to the collection, and update the TreeView. A new node for today's Year, Week, and Date should appear in the Navigation TreeView on the left. The form will return to `Browse` mode.

### Editing an Existing Entry

You can modify the content of any existing diary entry.

1.  **Select the entry:** In the Navigation TreeView, navigate to and click on the Date node (Level 3) of the entry you want to edit. The content will load into the memo.
2.  **Initiate Edit:** Click the `Edit` button in the top panel. Alternatively, you can press the `F2` key, or simply click anywhere within the Text Memo area *if the form is currently in `Browse` mode*. The form will switch to `Edit` mode.
3.  **Modify content:** Make changes to the text in the Text Memo Area.
4.  **Save changes:** Click the `Save` button in the top panel, or press `Ctrl + S` when the text memo has focus. Your modifications will be queued for persistence to the database, and the form will return to `Browse` mode.

### Deleting an Entry

You can remove a specific diary entry permanently.

1.  **Select the entry:** In the Navigation TreeView, navigate to and click on the Date node (Level 3) of the entry you want to delete. *(Note: You can only delete Date nodes (Level 3).)*
2.  **Initiate Delete:** Click the `Delete` button in the top panel.
3.  **Confirm Deletion:** A confirmation dialog box will appear, asking if you are sure you want to delete the selected diary page (e.g., "Delete diary page 11/15/2022 ?").
4.  **Finalize Deletion:** Click `Yes` to proceed with deleting the entry. The entry will be removed from the application and queued for deletion from the database. Click `No` to cancel the operation.

### Saving and Canceling Changes

When you are in `Insert` or `Edit` mode, you have options to either keep or discard your work.

*   **Saving:**
    *   While in `Insert` or `Edit` mode, with the Text Memo Area containing your desired content, click the `Save` button (in the top panel) or press `Ctrl + S` (when the memo has keyboard focus).
    *   The application will save the current content of the memo to the diary entry item. The item's status will be marked for adding or modification, and it will be placed in the internal delta queue for eventual writing to the database. The form will then switch back to `Browse` mode.
*   **Canceling:**
    *   While in `Insert` or `Edit` mode, if you decide you do not want to keep the changes you've made (or the new entry you started), click the `Cancel` button (in the top panel) or press the `Esc` key.
    *   Any changes made in the current `Insert` or `Edit` session will be discarded. If it was a new entry you were adding, the temporary item will be removed. The form will switch back to `Browse` mode, and the memo content will revert to what was last saved or selected.

### Searching Your Diary

The application allows you to search for specific text within your diary entries.

1.  **Initiate Search:** Click the `Search` button (in the top panel) or press `Ctrl + F`.
2.  **Enter Search Term:** An input box titled "Free text search" will appear. Type the word or phrase you are looking for in your diary entries.
3.  **Execute Search:** Click `OK` in the input box.
4.  **Review Results:** The application will search all diary entries (both their text content and date strings) based on your search term and the Case Sensitive / Match All settings configured in the Settings.
    *   If matches are found, the Navigation TreeView will automatically select the node corresponding to the *first* entry containing a match.
    *   The content of this entry will load into the Text Memo Area, and the *first* occurrence of your search term within that entry will be highlighted.
    *   The form will switch to `Search` mode, and the `Find Next` button will become enabled (unless this is the only match across all entries).

**Example: Performing a Simple Search**

Suppose you want to find all entries mentioning "meeting".

1.  Click the `Search` button.
2.  In the input box, type `meeting`.
3.  Click `OK`.
4.  The application searches. If an entry for 2023/Week 40/10/04/2023 contains "meeting", that date node will be selected in the TreeView, its text will load in the memo, and the first "meeting" in that memo will be highlighted. The status bar will show "Mode: Search".

### Using 'Find Next' in Search Mode

Once you have performed a search and the `Find Next` button is enabled, you can navigate through the results.

*   **Finding the next entry/match:** Click the `Find Next` button (in the top panel) or press `Alt + N`.
    *   If there are more matches in *subsequent* diary entries, the TreeView will select the node for the next entry with a match, and the first occurrence in that entry's memo will be highlighted.
    *   If there are more occurrences of the search term *within the currently displayed entry* but no more matches in subsequent entries, pressing `F3` (when the memo has focus) will cycle through the highlights within the current memo.
*   When there are no more matches left in the search results, the `Find Next` button will become disabled, and the form will eventually return to `Browse` mode.

### Returning Home

The "Home" action provides a quick way to reset the navigation view.

1.  **Initiate Home:** Click the `Home` button (in the top panel) or press the `Home` key (when the Text Memo Area does *not* have keyboard focus).
2.  **View Reset:** The Navigation TreeView will be reset. All nodes will collapse (except the Root node), the Root node will be selected, and then the TreeView will automatically select the node for the earliest (first) diary entry available.
3.  This action also exits `Search` mode if it was active.

### Accessing Settings

To customize the application's behavior:

1.  **Initiate Settings:** Click the `Settings` button in the top panel.
2.  **Open Settings Tab:** A new tab named "Settings" will open in the main window's tab control (`pctPages`).
3.  **Adjust Settings:** Interact with the controls within the Settings tab to change options like database location, backup path, backup frequency, batch update settings, and search sensitivity.
4.  **Save Settings:** Changes made in the Settings tab are typically saved immediately when you change them or when you close the application, depending on the specific setting control. The underlying `TDDSettings.Update` procedure writes changes to the `daily_diary.ini` file.
5.  **Close Settings Tab:** Close the Settings tab by clicking the 'x' button on the tab header (if visible and enabled) or by using any standard method for closing tabs in the Lazarus LCL framework.

## Technical Architecture

The project follows a multi-layered design:

1.  **User Interface (UI) Layer**: Handled primarily by the `lfm_dataaware.pas` unit (using Lazarus LCL forms and controls), this layer is responsible for presenting data to the user and capturing user input. It includes controls for navigation (`TTreeView`), text editing (`TMemo`), and action triggering (`TSpeedButton`). It observes changes in the BOM to update its display via the `TDDObserver` class.
2.  **Business Object Model (BOM) Layer**: Implemented in the `bom/bom_dd.pas` unit, this layer encapsulates the core business logic for managing diary entries (`TDDCollectionItem`). It acts as an interface (`IBom_dd`) between the UI and the data persistence layer. It manages a collection of diary items (`TCollection`), handles their lifecycle (creation, modification, deletion), and processes database updates via a delta queue (`TDDQueue`), allowing for optional batch updates (`fBatch`, `fUpdateCount`).
3.  **Data Persistence Layer**: This layer is responsible for interacting with the underlying database. The project uses `TLiteDb` (referenced in `bom/bom_dd.pas` and `global/daily_diary_const.pas`) and SQL commands defined in `global/daily_diary_const.pas`. The `bom/bom_dd.pas` unit directly interacts with this layer to perform CRUD operations and manage backups.
4.  **Settings Layer**: The `global/dd_settings.pas` unit handles application configuration, such as database file path, backup settings, and search options. Settings are persisted in an INI file (`daily_diary.ini`).
5.  **Search Layer**: The `bom/textsearch_dd.pas` unit provides free-text search capabilities across the diary entries managed by the BOM. It leverages a base text search engine (`bc_textsearch`, referenced) and operates on the stream content of `TDDCollectionItem` objects via the `OnEnumerateItemDD` callback method.
6.  **Utility Units**: Several other units (prefixed with `bc_`, e.g., `bc_datetime`, `bc_observer`, `bc_trvhelp`, etc.) provide common functionalities used across different layers, such as date/time handling, the observer pattern implementation, and TreeView helpers.

*(Note: The project also relies on Lazarus Form (`.lfm`) and Resource (`.res`) files, as indicated by `{$R *.lfm}` and `{$R *.res}` directives, which are not included in the source code snippets but are necessary for the visual form layout and compilation within Lazarus.)*

## Core Functionality Details (For Developers)

Detailed explanations of key application features from a developer's perspective, highlighting interactions between units:

### Data Management (CRUD Operations)

The `TDDCollection` class in `bom/bom_dd.pas` manages the lifecycle of `TDDCollectionItem` objects and their persistence.

*   **Reading Data**: The UI (`lfm_dataaware.pas`) calls `fBom.ReadDataWithBlob(false)`. This method connects to the database (`fDb.Connect`), clears the existing collection (`Clear`), starts a transaction, executes an SQL SELECT query (`SelSqlAsc` or `SelSqlDesc` from `daily_diary_const.pas`) to fetch all records including the BLOB text content, iterates through the results, creates a new `TDDCollectionItem` for each record (`Add_Dd`), populates its fields, loads the BLOB data into the item's `fText` stream, and adds the item to the collection. It commits the transaction and disconnects (`fDb.DisConnect`). After loading, the `ShowDataRead` method in `lfm_dataaware.pas` populates the `TTreeView`.
*   **Adding New Entries**: When the "Add" button is clicked (`btnAddClick` in `lfm_dataaware.pas`), the UI checks if a date node exists for today. If not, it calls `fBom.CreateNew`. This creates a `TDDCollectionItem` without an owner (`TDDCollectionItem.Create(nil)`), sets its initial date, and returns it. The UI sets its `fModified` status to `mAdded` and appends it to the BOM's delta queue using `fBom.AppendToDelta`. The `AppendToDelta` method enqueues the item and potentially triggers a database write via `DoUpdate`. `DoUpdate` processes `mAdded` items by calling `AddRecord`. `AddRecord` connects, starts a transaction, uses a prepared INSERT SQL statement (`InsSql` from `daily_diary_const.pas`), binds parameters (date, datestr, weeknumber, text BLOB from stream, reserved), executes the SQL, retrieves the `LAST_INSERT_ROWID()` or `id_dd`, assigns it to the item (`anItem.Id_DD`), commits the transaction, disconnects, and sets the item's status to `mNone`. The UI is notified (`NotifyObservers`) to add the new item to the TreeView.
*   **Editing Existing Entries**: When an entry is selected and the form is put into Edit mode, the UI holds a reference to the `TDDCollectionItem` (`fEditItem`). When the "Save" button is clicked (`btnEditSaveClick` in `lfm_dataaware.pas`), the UI updates the item's `fText` stream from the memo's content. It sets the item's `fModified` status to `mAltered` and appends it to the delta queue via `fBom.AppendToDelta`. `AppendToDelta` enqueues and potentially triggers `DoUpdate`. `DoUpdate` processes `mAltered` items by calling `UpdateRecord`. `UpdateRecord` connects, starts a transaction, uses a prepared UPDATE SQL statement (`UpdSql` from `daily_diary_const.pas`), binds parameters (updated fields + `id_dd`), executes the SQL, commits, disconnects, and sets the item's status to `mNone`. The UI is notified (`NotifyObservers`) of the change.
*   **Deleting Entries**: When a date node is selected and the "Delete" button is clicked (`btnDeleteClick` in `lfm_dataaware.pas`), the UI retrieves the corresponding `TDDCollectionItem`, sets its `fModified` status to `mDelete`, and appends it to the delta queue via `fBom.AppendToDelta`. For `mDelete` items, `AppendToDelta` forces an immediate update (`UpdateData(true)`). `UpdateData` calls `DoUpdate`. `DoUpdate` processes `mDelete` items by calling `DeleteRecord`. `DeleteRecord` connects, starts a transaction, uses a prepared DELETE SQL statement (`DelSql` from `daily_diary_const.pas`) with the item's `id_dd`, executes the SQL, commits, and disconnects. `DoUpdate` then calls `DeleteItem` on the collection, which finds the item by ID and removes it from the in-memory collection, freeing the item object. The UI is notified (`NotifyObservers`) to remove the node from the TreeView.

Database updates are managed through a delta queue (`TDDQueue`). Changes (`mAdded`, `mAltered`, `mDelete` defined in `bom/bom_dd.pas`) are enqueued. The `DoUpdate` procedure processes this queue. Updates are persisted either immediately (forced by `UpdateData(true)`, e.g., on delete or shutdown) or in batches when the queue count reaches `DDSettings.BatchCount` (if `DDSettings.BatchUpdates` is true).

### Navigation

The `TTreeView` (`trvNav`) structure is built and managed by the `lfm_dataaware.pas` unit.

*   The `ShowDataRead` procedure iterates through the loaded `TDDCollection` and adds nodes for each Year (Level 1), Week Number (Level 2), and Date (Level 3). Date nodes store a pointer to the corresponding `TDDCollectionItem` in their `Data` property. Helper functions like `bcGetNodeByTextAtLevel` and `AddChildNodeWithData` (presumably from `bc_trvhelp`) are used.
*   The `trvNavSelectionChanged` event handler updates the memo content when a Date node (Level 3) is selected by loading the `Text` stream from the associated `TDDCollectionItem`'s `Data` pointer.
*   The `btnAddClick` and `btnDeleteClick` procedures also interact with the TreeView to find or remove nodes.
*   The `btnHomeClick` procedure collapses and re-expands the TreeView and selects the first date node.

### Text Search

The `TddTextSearch` class in `bom/textsearch_dd.pas` performs searches.

*   The `btnSearchClick` procedure in `lfm_dataaware.pas` reads search settings (`CaseSensitive`, `MatchAll`) from `DDSettings` and the search pattern from an input box. It calls `fFts.SearchDataset(mask, ...)` on the `TddTextSearch` instance (`fFts`).
*   `TddTextSearch.SearchDataset` clears previous results, gets the item count from the BOM (`fDataset.ItemCount`), and calls `fDataset.EnumerateDD`. This method in `bom/bom_dd.pas` iterates through the `TDDCollection` and calls the `TddTextSearch.OnEnumerateItemDD` callback for each `TDDCollectionItem`.
*   `OnEnumerateItemDD` loads the diary text from the item's stream, concatenates it with the item's date string, and uses the `bcFindMatches` function (presumably from `bc_advstring`) to find occurrences of the search pattern. If matches are found, a `TTextSearchRec` containing the item ID, pattern, match count, length, item name (date string), and match positions (`srPos`) is added to the search results list (`AddMatch`).
*   After `SearchDataset` returns, the `btnSearchClick` and `btnFindNextClick` procedures navigate the results list (`fFts.ResetCursor`, `fFts.GetNextSearchRec`). For each match, they find the corresponding Date node in the TreeView (`bcGetNodeByTextAtLevel`) and highlight the match within the memo (`memText.SelStart`, `memText.SelLength`) using the positions stored in `TTextSearchRec.srPos`.

### Settings and Backup

Configuration and backup are handled by `global/dd_settings.pas` and `bom/bom_dd.pas`.

*   The `TDDSettings` class (`dd_settings.pas`) uses `TIniFile` to read/write settings to `daily_diary.ini` (located by `DD_Inifilename` in `daily_diary_const.pas`). It provides properties like `Databasename`, `BackupPath`, `AutoBackup`, `BackupFrequency`, etc., accessed globally via the `DDSettings` function. The `Update` procedure saves changes to the INI file.
*   The `btnSettingsClick` procedure in `lfm_dataaware.pas` creates or shows a settings frame (`tfm_settings`) and links its changes back to the `DDSettings` instance via the `OnSettingsChange` event handler.
*   The `TDDCollection.BackupData` procedure (`bom/bom_dd.pas`) creates a file copy of the database file (`DDSettings.Databasename`) to the location specified by `DDSettings.BackupPath`, appending the date to the filename.
*   Autobackup is triggered during `TDDCollection` creation if `DDSettings.AutoBackup` is true. The `CheckAutobackup` procedure reads the last backup date from the `autobackup` database table (`SelSqlAB` from `daily_diary_const.pas`) and compares it to the current date based on the `BackupFrequency`. If due, `BackupData` is called, and `UpdateAutobackup` inserts a new record into the `autobackup` table (`InsSqlAB` from `daily_diary_const.pas`), updating `DDSettings.LastBackup`.

## Database Schema

The Daily Diary project uses an SQLite database. Based on the SQL constants in `global/daily_diary_const.pas`, the primary tables are:

*   **`daily_diary`**: Stores the actual journal entries.
    *   `id_dd` (integer primary key): Unique identifier.
    *   `date_dd` (integer): Date serial number.
    *   `datestr_dd` (varchar(10)): Formatted date string.
    *   `weeknumber_dd` (integer): ISO Week number.
    *   `text_dd` (blob): Diary entry content (text stream).
    *   `reserved_dd` (varchar(512)): Reserved text field.

*   **`autobackup`**: Records the dates of automatic backups.
    *   `id_ab` (integer primary key): Unique identifier.
    *   `date_ab` (integer): Date serial number of backup.
    *   `frequency_ab` (integer): Configured frequency at backup time (0: daily, 1: weekly, 2: monthly).
    *   `reserved_ab` (varchar(512)): Reserved text field, used for backup date string.

Note: SQL commands for `dd_photos` (`CreatePH`, `InsSqlPH`, `DelSqlPH`, `SelSqlPH`) and `dd_pictures` (`CreatePI`, `InsSqlPI`, `DelSqlPI`, `SelSqlPI`) tables exist in `global/daily_diary_const.pas`, suggesting planned image functionality, but these tables and associated logic are not fully implemented in the provided `bom/bom_dd.pas` and `bom/bom_imgdd.pas` units.

## Project Structure

The project is organized into several Pascal units (`.pas` files) and associated files:

*   **`daily_diary.lpr`**: The main program file. Initializes the LCL application and creates the main form (`TfrmDataAware`).
*   **`lfm_dataaware.pas`**: The main form unit. Contains the user interface definition and handles user interactions, orchestrating calls to the BOM and other logic. Uses helper functions from `bc_trvhelp` and `bc_pcthelp`.
*   **`bom/bom_dd.pas`**: Implements the `TDDCollection` class and `IBom_dd` interface, representing the Business Object Model for diary entries. Manages the collection of `TDDCollectionItem` objects, handles the delta queue (`TDDQueue`), and interacts with the database layer (`TLiteDb`) and SQL constants (`daily_diary_const.pas`). Uses `bc_datetime`, `bc_mtlinklist`, `bc_observer`, `bc_memdataset`, `bc_msgqueue`, `DateUtils`.
*   **`bom/bom_imgdd.pas`**: An incomplete unit (currently empty implementation) seemingly intended for handling images within diary entries, potentially utilizing the `dd_photos` and `dd_pictures` database tables. Uses `bc_baselist`.
*   **`bom/textsearch_dd.pas`**: Implements the `TddTextSearch` class, specializing the generic `bc_textsearch` engine for searching within `TDDCollectionItem` objects. Interacts with the BOM via the `IBom_dd` interface. Uses `bc_advstring`, `LazUtf8`.
*   **`global/daily_diary_const.pas`**: Defines global constants, including all SQLite SQL statements for database operations, application metadata (title, version), keyboard codes, dialog results, and utility functions for determining standard file paths (`DD_Databasename`, `DD_Inifilename`, `DD_AdjustTrailingSlash`). Uses `Classes`, `SysUtils`.
*   **`global/dd_settings.pas`**: Implements the `TDDSettings` class for managing application configuration loaded from and saved to an INI file (`daily_diary.ini`). Uses `Inifiles`, `daily_diary_const`, `bc_errorlog_mt`, `bc_msgqueue`.

## Configuration Details

The application's configuration is managed through the `daily_diary.ini` file, located by default in the same directory as the executable (`./`), as defined by `DD_Inifilename` in `global/daily_diary_const.pas`.

*   **`[Files]` Section:**
    *   `DatabaseName`: Specifies the full path to the SQLite database file. Default: `./db/daily_diary.db3`.
    *   `BackupName`: Defines the directory path where automatic backup files will be stored. If set to 'Not defined', autobackup is effectively disabled regardless of the `AutoBackup` setting. Default: `Not defined`.
    *   `AutoBackup`: A boolean value (`true` or `false`) to enable or disable the automatic backup feature. Default: `false`.
    *   `BackupFrequency`: An integer controlling how often automatic backups occur when enabled.
        *   `0`: Daily backup.
        *   `1`: Weekly backup (based on ISO week number).
        *   `2`: Monthly backup.
        Default: `0` (Daily).
    *   `LastBackup`: A string storing the date of the most recent successful automatic backup. This value is managed internally by the application. Default: `01.01.1970`.
*   **`[Engine]` Section:**
    *   `BatchUpdates`: A boolean value (`true` or `false`) to enable or disable batch processing of database updates. When true, changes are cached and written to the database in groups. Default: `false`.
    *   `BatchCount`: An integer specifying the number of queued updates that trigger a database write operation when `BatchUpdates` is true. Default: `3`.
    *   `CaseSensitive`: A boolean value (`true` or `false`) determining if the text search is case-sensitive. Default: `false` (case-insensitive).
    *   `MatchAll`: A boolean value (`true` or `false`) controlling the text search behavior. If true, all occurrences of the search term within an entry are considered matches. If false, only the first match within an entry is counted. Default: `true`.

## Enhancing Daily Life: Ideas for Extending the Project

At its core, a diary application helps users remember, reflect, and track their lives. It can be a powerful tool for:

*   **Memory Aid:** Recording daily events prevents details from being lost to time.
*   **Personal Growth & Reflection:** Writing down thoughts and feelings can provide clarity and insight.
*   **Progress Tracking:** Documenting activities related to hobbies, health, or projects helps monitor advancement.
*   **Logbook Creation:** Maintaining a detailed record of specific events (e.g., health symptoms, technical challenges, creative ideas) can be invaluable for analysis or problem-solving later.

The current Daily Diary project provides a solid foundation for these uses with its basic text entry and chronological organization. However, by extending the application with features hinted at in the codebase or commonly found in robust journaling tools, its capability to solve real-world problems can be significantly enhanced. Below are ideas for potential future development, leveraging the existing architecture.

*   **Adding Rich Content (Images & Rich Text):**
    *   **Problem Solved:** Current entries are plain text, limiting expressiveness and the ability to capture visual memories. Users might need to remember visual details or context associated with an entry.
    *   **Enhancement:** Implementing the image handling suggested by the `bom/bom_imgdd.pas` unit and the `dd_photos`/`dd_pictures` database tables would allow users to attach photos directly to entries. This transforms the diary into a multimedia journal, making memories richer and easier to recall visually. Furthermore, upgrading the `TMemo` to a RichMemo or similar control and utilizing the BLOB field's capacity to store formatted text (like RTF) would enable users to structure their thoughts with headings, bold text, lists, etc., improving readability and organization within individual entries.
    *   **Technical Path:** The `bom/bom_imgdd.pas` unit needs its implementation completed, including methods for adding, loading, and deleting images, interacting with the `dd_photos` and `dd_pictures` tables. The UI (`lfm_dataaware.pas`) would require controls for adding/viewing images and potentially a RichMemo component. The data persistence logic in `bom/bom_dd.pas` for the `text_dd` BLOB field should be compatible with saving formatted text streams, but the loading/saving in the UI would need updating.

*   **Enhanced Search, Tagging, and Categorization:**
    *   **Problem Solved:** Finding specific information within a large volume of text entries can be difficult, even with basic text search. Users may want to group entries by theme, topic, or importance, not just date.
    *   **Enhancement:** Building upon the existing `TddTextSearch`, implement more advanced search criteria (e.g., date range filters). Introduce a tagging system, potentially utilizing the `reserved_dd` field in the `daily_diary` table or adding a dedicated `tags` table. This would allow users to assign keywords (like #projectX, #health, #family) to entries. The UI would need input fields for tags and new navigation/filtering options (e.g., a list of tags to click on) to quickly retrieve all entries associated with a specific tag or category.
    *   **Technical Path:** Modify the `daily_diary` table schema (or utilize `reserved_dd`) or add a new `tags` table and a linking table (`entry_tags`). Update the `TDDCollection` in `bom/bom_dd.pas` to handle saving and loading tag data. Enhance the `TddTextSearch` to include tag filtering in its search logic. The UI (`lfm_dataaware.pas`) needs controls for adding/displaying tags and implementing the filtering logic based on user selection.

*   **Data Import/Export and Portability:**
    *   **Problem Solved:** Users may want to migrate their diary data from another application, back up their data in a universally readable format, or share specific entries outside the application.
    *   **Enhancement:** Add functionality to import entries from common formats (e.g., plain text files with date markers, or CSV/JSON if a structured format is defined) and export entries to formats like plain text, Markdown, RTF, or HTML. This makes the diary data more portable and reduces vendor lock-in.
    *   **Technical Path:** Implement new procedures within `bom/bom_dd.pas` or a new utility unit to handle file parsing (import) and formatting (export) for different file types. These procedures would interact with the `TDDCollection` to read/write data. The UI (`lfm_dataaware.pas`) needs menu items or buttons to trigger these import/export operations, potentially involving file dialogs.

*   **Improved Navigation and Visualization:**
    *   **Problem Solved:** The current TreeView is functional but can become long for extensive diaries. Users might prefer alternative ways to browse their history.
    *   **Enhancement:** Supplement the TreeView with a calendar view that visually indicates days with entries. Clicking a day on the calendar could navigate the TreeView or directly load the entry for that date. This provides a more intuitive, glanceable overview of writing frequency and past events.
    *   **Technical Path:** Implement a new visual component (a calendar control) in the UI (`lfm_dataaware.pas`). This component would need to query the `TDDCollection` (or the database directly via `bom/bom_dd.pas`) to determine which dates have entries and update its display accordingly. Logic would be needed to handle user clicks on the calendar to navigate to the corresponding date entry.

*   **User Authentication and Data Security:**
    *   **Problem Solved:** Diary entries are highly personal. The current application lacks security features to protect sensitive information from unauthorized access.
    *   **Enhancement:** Implement a password protection mechanism upon application launch. Consider encrypting the SQLite database file (`daily_diary.db3`) itself to secure the data at rest.
    *   **Technical Path:** Add user authentication logic (e.g., a login form, password hashing) at startup (`daily_diary.lpr` or `lfm_dataaware.pas`). Investigate SQLite encryption extensions or external encryption libraries compatible with Free Pascal to encrypt/decrypt the database file when opening/closing it within `bom/bom_dd.pas`'s `TLiteDb` interactions.
