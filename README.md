# Download RStudio
  - If you don't have [RStudio](https://posit.co/download/rstudio-desktop/) 
# Step to Reproduce
 ## 1.Create a new project with RStudio
      1. Once open RStudio IDE, start navigate on File > New Directory > New Project
        [!Text](./images/pic1.png)
        [!Text](./images/pic2.png)
      2. Choose a directory for your R Project
  ## 2. Navigate in Terminal to create new Project
      1. Click Terminal (where have Console and Background Jobs tabs)
      2. Navigate through directory with "cd"" command to 
        * If in Users, but file in ~/Users/Desktop/[project_name], then we type "cd Desktop/[project_name]"
      3. Once in right [project_name] directory, type git "git clone https://github.com/stevenmach123/role-management-chatroom.git"
          * Once done cloning; on Files on right, we see "power_chicago" directory 
          [!Text](./images/pic4.png) 
      4. Type "cd power_chicago" to went in "power_chicago" directory 
          * Note: I am in power_chicago as my current project, so I have path User/Desktop/power_chicago/power_chicago. But your [project_name] can be different
          [!Text](./images/pic3.png) 
  ## 3.Run Project
      1. When at "power_chicago" directory, click pro3_1.R to view up the file code
      2 Click "Run" on Shiny App. Then just wait for data to process and view.