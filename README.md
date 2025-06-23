# Short Overview

This web application will help you to orient yourself in the upcoming [JSP 2005 meeting](https://ww2.amstat.org/meetings/jsm/2025/). All information obtained by scrapping the official program [website](https://ww3.aievolution.com/JSMAnnual2025/Events/pubSearchOptions?style=0) is presented in a nive gpraphical way. Yuo can also easily select and save you are interested in and plan to visit, filer out some other events, inspect the details of each of the sessions, etc.

This application was created using R Shiny service. All the code is avialably on the [GitHub page](https://github.com/ALuchinsky/jsm_schedule).

# Page Layout

Typical page layout is shown on the figure below. As you can see, it consists of several blocks:

* **Control buttons**, namely:
  * **Reset**: Resets the application state completely
  * **Info**:  Prints some internal information to the R console. This button is for debug pupropse only and is not helpful for online usage
  * **Redraw**: Redraws the main Time Schedule region
  * **Download**: Allows you to download a text file with selected events
  * **Upload**: Allows you to upload this file to continue you work on the scheduling later
  * **Options**: Helps you to select which types of events are visible
* **Option controls**, namely:
  * **Wrap width** slidebar: allows you to select the wrap length of the event card
  * **Active day selector**: allows you to choose what days to display. Note that it is possible to display several days, even all o them, but the time table visualization will look messy in this case
  * **Text filter input box**: Allows you to filter some events either by event title or session number.
* **Time Table pane**: This is the main part of the application, all filteres events are shown here. More information will be presented below
* **Event type selector**: You can check whish of the section types to show in the table pane.

# Functionality

Usually you start your work with selected some day and inspecting all available at this day events. Each of such events is displayed as a rectangular tile with event title shown in it. Each tile is colored according to the event type. Some short details about this event are shown when hovering the mouse over the tile. More details (e.g. list of talks that will be presented) will be shown in the dialog window which is opened when you right-click the event tile.

Of course, the JSM program is very tight, so it makes sence to restrict the event types you are interested in. This is done by the event type selector, i.e. a series of check-boxes located on the right. Note that these boxes are also colored in accordance with the event type, and you can select several of them at the same time. You can also filtered out some events by using text filter input box, located above the time table pane.

You can obtain more information about the specific event by right-clikcing it. As a result such details as event room, number of talks, titles and presenters of these talks,  will scrapped from the web and displayed in the opened dialog box. Note that all scrapped information is saved, so there will be no need to look for it again after it was downloaded.

After you have chosen the event, you can select it by double-clicking with your left mouse button. The title of the selected event will be shown capitalized and all other sections with intersecting time slots will be grayed out and made inactive. After double-clicking once again, the selected event will be un-selected.

Using options from the Options dialog box you can decide whether to show any of the Inactive, Selected, or active non-selected events. This will help you to make the display less clattered, figure out which events you have selected as interested for you personally, and what other events could be added to your time schedule without time conflicts.

This list of your selected events can be saved to local file using the Download button. In this simple text file for each of the dates all you selected events will be listed, including such details as time, title, section number, room location etc. You can download this saved file back into web session to continue your work on the scheduling.

# Typical Work Process

The following order of schdule selection could be adopted:

* Select one specific date of the conference and some event types you are interested in
* You can also use the filter text box to do more specific search of the sessions
* After inspecting the events select the ones you like most by double-clicking that
* In the Options dialog box turn off display of the shadowed events to remove all time conflicts
* Continue selecting more sessions at your wish (yous can coinsuder some other days, event types, etc)
* Turn of display of the selected events to see if there are any other options for events that you can schedule without time conflict
* Switch display only to selected events to observe you time schedule
* Save the list of your seletced events to a text file

Later you can download this text file back into the application, modify your choice, etc.





