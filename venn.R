library(VennDiagram)

nrow(subset(random, sleep_13_p==1))
nrow(subset(random, sleep_14_p==1))
nrow(subset(random, sleep_15_p==1))

nrow(subset(random, sleep_13_p==1 & sleep_14_p ==1 ))
nrow(subset(random, sleep_14_p==1 & sleep_15_p ==1 ))
nrow(subset(random, sleep_13_p==1 & sleep_15_p ==1 ))

nrow(subset(random, sleep_13_p==1 & sleep_14_p ==1 & sleep_15_p ==1))




draw.triple.venn(area1 = 9678, area2 = 9932, area3 = 6040, n12 = 9630, n23 = 6008, n13 = 5945, 
                 n123 = 5931, category = c("Breathing difficulty during sleep", "Gasping", "Snoring"), lty = "blank", 
                 fill = c("skyblue", "pink1", "mediumorchid"))



library(VennDiagram)

nrow(subset(random, sleep_13_p==2))
nrow(subset(random, sleep_14_p==2))
nrow(subset(random, sleep_15_p==2))

nrow(subset(random, sleep_13_p==2 & sleep_14_p ==2 ))
nrow(subset(random, sleep_14_p==2 & sleep_15_p ==2 ))
nrow(subset(random, sleep_13_p==2 & sleep_15_p ==2 ))

nrow(subset(random, sleep_13_p==2 & sleep_14_p ==2 & sleep_15_p ==2))




draw.triple.venn(area1 = 328, area2 = 148, area3 = 2499, n12 = 83, n23 = 50, n13 = 125, 
                 n123 = 33, category = c("Breathing difficulty during sleep", "Gasping", "Snoring"), lty = "blank", 
                 fill = c("skyblue", "pink1", "mediumorchid"))


nrow(subset(random, sleep_13_p==3))
nrow(subset(random, sleep_14_p==3))
nrow(subset(random, sleep_15_p==3))

nrow(subset(random, sleep_13_p==3 & sleep_14_p ==3 ))
nrow(subset(random, sleep_14_p==3 & sleep_15_p ==3 ))
nrow(subset(random, sleep_13_p==3 & sleep_15_p ==3 ))

nrow(subset(random, sleep_13_p==3 & sleep_14_p ==3 & sleep_15_p ==3))




draw.triple.venn(area1 = 92, area2 = 37, area3 = 940, n12 = 20, n23 = 15, n13 = 25, 
                 n123 = 9, category = c("Breathing difficulty during sleep", "Gasping", "Snoring"), lty = "blank", 
                 fill = c("skyblue", "pink1", "mediumorchid"))

nrow(subset(random, sleep_13_p>3))
nrow(subset(random, sleep_14_p>3))
nrow(subset(random, sleep_15_p>3))

nrow(subset(random, sleep_13_p>3 & sleep_14_p >3 ))
nrow(subset(random, sleep_14_p>3 & sleep_15_p >3 ))
nrow(subset(random, sleep_13_p>3 & sleep_15_p >3 ))

nrow(subset(random, sleep_13_p>3 & sleep_14_p >3 & sleep_15_p > 3))

134+60+1601

draw.triple.venn(area1 = 42, area2 = 23, area3 = 661, n12 = 19, n23 = 18, n13 = 32, 
                 n123 = 16, category = c("Breathing difficulty during sleep", "Gasping", "Snoring"), lty = "blank", 
                 fill = c("skyblue", "pink1", "mediumorchid"))
