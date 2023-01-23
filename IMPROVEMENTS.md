## Improvements for CTUNetwork 0.1.2

# DLFReached
-   The date when DLF was reached cannot be retrieved from Projectfacts backend data (i.e., in UI under "Project details" --> "Insel specific" --> "DLF reached", hence this has to be recomputed. 
Currently this options is deactivated because the way I have implemented it largely reduces the performance of the App (the computation is too heavy and would require improvements).
-   R/filterData.R (lines 74-88): Where the computation is programmed (currently commented out)
-   inst/shinyApp/server.R (lines 94-103): Where the filtering is applied based on UI input (curently commented out)
-   Correct behavior: DLF reached should be computed and used to filter the data. 

# Default settings
-   Defaults are correctly saved and reloaded (also inside a volume for the docker container) but they are not applied directly to the network graph when starting the App.
Hence, to apply the changes that were previously made to the default settings, one has to open the "Graph parameter" tab.
-   Correct behavior: Changes to defaults should be applied automatically to the network graph when the App is started.

# AllData() Shiny reactive component
-   inst/shinyApp/server.R  (lines 32-182): I should probably have split the "AllData <- shiny::reactive" components into smaller, isolated reactive ones. 
This would have avoided taking into account the order of the sequence of data processing.
-   Correct behavior: The App should run the same but with AllData() being split into multiple smaller components.