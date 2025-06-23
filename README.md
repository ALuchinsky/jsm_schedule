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

Of course, the JSM program is very tight, so it makes sence to restrict the event types you are interested in. This is done by the event type seletcor, i.e. a series of check-boxes located on the right. Note that these boxes are also colored in accordance with the event type, and you can select several of them at the same time. You can also filtered out some events by using text filter input box, located above the time table pane.

