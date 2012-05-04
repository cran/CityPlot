\name{CityPlot}
\alias{CityPlot}
\title{ CityPlot }
\description{
CityPlot generates an extended version of a traditional entity-relationship diagram for a database.
It provides a combined view of database structure and contents.\cr
The graphical output applies the metaphor of a city (that's why it's called CityPlot):\cr
Each entity corresponds to a building, relationships between entities correspond to the underground distribution network of a city.
\cr
The size of an entity box is adjusted according to available database contents for this entity:\cr 
Wide boxes represent entities with many attributes, high boxes represent entities with many records.\cr
Each available data point is represented by a colored small spot within the corresponding entity box.\cr 
Each column of spots corresponds to a certain attribute and is colored according to its data type.\cr
Numeric data points are colored in blue, categorical in green, date/time in yellow (time: light yellow, date: dark yellow), others in red. 
White spots correspond to missing data.\cr\cr
   Input:  A csv-file for each database table and a controlfile describing relations between tables (see examples)\cr
   Output: An extended ER diagram
}
\usage{ CityPlot(controlfile = "", pdfmode = "",shownames=T) }
\arguments{
  \item{controlfile}{
   File name of controlfile describing database tables and their relations.\cr
   File format: see examples. If omitted, file.choose()-dialog is started
  }
  \item{pdfmode}{
   value "y" or "Y": graphical output as pdf-file
  }
  \item{shownames}{
   value T or F: display names of entities
  }
}
\references{
   CityPlot: colored ER diagrams to visualize structure and contents of databases (manuscript submitted)
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
   \dontrun{
CityPlot(controlfile=file.path(path.package("CityPlot"),"extdata","ER_control.txt"))
   }
}
\keyword{ ER diagram }
\keyword{ plot }
