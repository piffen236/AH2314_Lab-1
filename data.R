## Read in and prepare data here.
##
## For the data of Lab 1, with filename "varset2.rds", a mode availability
## matrix named 'avail' needs to be created. Each row of avail corresponds to
## an observation in the data set, and the six columns corresponds to the
## modes as coded in the MODE variable. A cell in the avail matrix should
## indicate with TRUE/FALSE if the mode is available for the observation.
##
varset3 <- readRDS("~/Documents/R School/Lab 1/varset3.rds")
## create  Mtrix determining what is available, this is a T/F matrix
avail <- data.frame(varset3$BILOK,varset3$PASSOK,varset3$BUSSOK,varset3$TAGOK,varset3$GANGOK,varset3$CYKELOK)
avail <- setNames(avail, c("1", "2", "3", "4", "5", "6"))
avail <- avail == 1 

library(ggplot2)

tid = data.frame(varset3$BILTID,varset3$CYKELTID,varset3$BUSSTID,varset3$GANGTID,varset3$PASSTID,varset3$TAGTID)

## --- BOXPLOT OF TRAVELTIME BY MODE --- ###

# Rename columns
names(tid) <- c("Car time", "Bike time", "Bus time", "Walk time", "Passanger time", "Train time")

p <- ggplot(stack(tid), aes(x = values, y = ind)) + xlab("Travel time [Minutes]") + ggtitle("Travel time foe different modes of transportation") + ylab("Travel Mode") + geom_boxplot()
p + theme(
plot.title = element_text(size=10, face="bold.italic"),
axis.title.x = element_text(size=14, face="bold"),
axis.title.y = element_text(size=14, face="bold")
)


## --- PIE CHART OF MODE CHOICES --- ###
## counting how many times each mode is taken
mode_count <- as.data.frame(table(varset3$MODE))
print(mode_count)
## column names
colnames(mode_count) <- c("Mode", "Count")

# Rename the Modes
mode_count$Mode <- factor(mode_count$Mode,
                          levels = c("1", "2", "3", "4", "5", "6"),
                          labels = c("Car", "Car pass.", "Bus", "Rail", "Walk", "Bike"))

pastel_colors <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF", "#babaff")

ggplot(mode_count, aes(x = "", y = Count, fill = as.factor(Mode))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  scale_fill_manual(values = pastel_colors) +  labs(title = "Distribution of Modes",
       x = NULL,
       y = NULL,
       fill = "Mode")
