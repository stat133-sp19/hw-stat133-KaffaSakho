data-dictionary
================

**team\_name**: A character vector that describes the name of player's team. Example: "Golden State Warriors"
**game\_date**: A character vector that describes the date the game took place in the form MM/DD/YY . The range of the date falls in the 2016 basketball season. Example: 3/24/17
**season**: An integer vector that describes the year the game took place. NBA games take place during yearly seasons. The only possible value here is 2016 since the data represents statistics from the 2016 season.
**period**: An integer vector that describes an NBA game is divided in 4 periods of 12 mns each. For example a period of 1 refers to the first period (the first 12 mns of the game).
**minutes\_remaining**: An integer vector that shows the number of minutes remaining in the current period. The range of values is 0 to 12.
**seconds\_remaining**: An integer vector that shows the nnumber of seconds remaining in the current period. The range of values is 0 to 60.
**shots\_made\_flag**: A factor or character vector that indicates whether a shot was made or missed.Possible values are y or n or (after modification) shot\_yes or shot\_no. y and shot\_yes indicate that the shot was made while n and shot\_no indicate that the shot was missed.
**action\_type**: A character of factor vector that has to do with the basketball moves used by players either to pass by defenders to gain access to the basket, or to get a clean pass to a teammate to score a two pointer or three pointer. Example: "Alley Oop Dunk Shot"
**shot\_type**: A character vector that indicates whether a shot is a 2-point field goal, or a 3-point field goal.
**shot\_distance**: A real vector that describes the distance to the basket (measured in feet).
**opponent**: A character vector that describes the opponent NBA team being played. Example: "Sacramento Kings"
**x**: an numeric vector that indicates the x-coordinate of where the shot occured, in inches. Example: -2
**y**: a numeric vector that indicates the y-coordinate of where the shot occured, in inches. Example: 132
**name**: a character vector indicating the name of the player. Example: Andre Iguodala.
**minute**: an integer vector indicating the minute (from the start of the game) when the shot occured. Example: 36.
