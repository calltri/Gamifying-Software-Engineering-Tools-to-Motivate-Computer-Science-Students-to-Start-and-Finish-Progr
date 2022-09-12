library(ggplot2)
library(sqldf)
#*****************************#
#Manipulating data structures for ease of analysis
#****************************#
#setwd("R")
l_assignment <- read.csv("mdl_block_leaderboard_assignment.csv")
roster <- read.csv("Section Roster.csv")
l_travis <- read.csv("travis_builds_no_initial_commits.csv")
forum <- read.csv("mdl_block_leaderboard_forum.csv")
section1 <- roster[1:25,]
section2 <- roster[26:50,]
repo_creation <- read.csv("repo_creation_early_df.csv")
repo_first_clone <- read.csv("repo_first_clone_early_df.csv")
total_commmits <- read.csv("total_commits_df.csv")
#<- read.csv("travis_builds_no_initial_commits.csv")


l_travis_size <- 1855

#use current course data only in assignment table
l_assignment <- sqldf("SELECT * FROM l_assignment WHERE courseid = 6") 

#create database of timestamps
pas <- c("pa1", "pa2","pa3","pa4","pa5","pa6","pa7","pa8","pa9")
time <- c(1579766399, 1580975999,1582012799,1582876799, 1585119599, 1585724399, 1586933999, 1587711599, 1588316399)
pa_timestamps <- data.frame(pas, time)
pa_timestamps$s1_num_commits <- 0
pa_timestamps$s2_num_commits <- 0

#modify l_travis to add timeearly and timedue variables, as well as their values
l_travis$timeearly <- 0
l_travis$timedue <- 0
for(i in 1:9) {
  l_travis <- within(l_travis, {
    timedue[pa == pa_timestamps$pas[i]] <- pa_timestamps$time[i]
  })
}
l_travis$timeearly <- l_travis$timedue - l_travis$commit_timestamp
l_travis$timeearly[l_travis$timeearly < 0] <- 0

#modify l_assignment to add timeearly and timedue variables, as well as their values
l_assignment$timeearly <- 0
l_assignment$timedue <- 0
for(i in 1:9) {
  l_assignment <- within(l_assignment, {
    timedue[modulename == pa_timestamps$pas[i]] <- pa_timestamps$time[i]
  })
}
l_assignment$timeearly <- l_assignment$timedue - l_assignment$timefinished
#set negative values to 0, as they are no longer early
l_assignment$timeearly[l_assignment$timeearly < 0] <- 0

#put timestamps in order
l_travis <- l_travis[order(l_travis$commit_timestamp),]

#compile various student specific data in roster
roster$totalcommits <- 0
for (i in 1:50) {
  x = 0
  for(j in 1:l_travis_size) {
    if (l_travis$github_assignment_acceptor[j] == roster$githubusername[i]) x <- x + 1
  }
  roster$totalcommits[i] <- x
}
roster$totalpoints <- 0
for (i in 1:50) {
  x = 0
  for(j in 1:344) {
    if (l_assignment$studentid[j] == roster$userid[i]) x <- x + l_assignment$pointsearned[j]
  }
  roster$totalpoints[i] <- x
}
roster$testspassed <- 0
for (i in 1:50) {
  x = 0
  for(j in 1:344) {
    if (l_assignment$studentid[j] == roster$userid[i]) x <- x + as.numeric(l_assignment$testspassed[j])
  }
  roster$testspassed[i] <- x
}
roster$forumresponses <- 0
for (i in 1:50) {
  x = 0
  for(j in 1:239) {
    if (forum$studentid[j] == roster$userid[i]) x <- x + 1
  }
  roster$forumresponses[i] <- x
}


#find number of 1:1 commits (ie: student assed only 1 additional test over their previous number)
#for each student and each PA need curr commit, prev commit, and see if there is a diff of 1
roster$count_1_test_passed <- 0
for (i in 1:50) {
  #reset for every student
  number_1_test <- 0
  for(j in 1:9) {
    #reset for every pa
    prev_tests_passed <- 0
    for (k in 1:l_travis_size) {
      #for all students, pa's, and commits, if a commit is a given students under a given pa
      if (l_travis$github_assignment_acceptor[k] == roster$githubusername[i]
          && l_travis$pa[k] == pa_timestamps$pas[j])
        #if 1 test is passed compared to the prev commit, add 1
      {(if (prev_tests_passed + 1 == l_travis$passed_tests[k]) number_1_test <- number_1_test + 1)
        #assign current commit passed_tests value to prev tests passed  
        if(l_travis$passed_tests[k] > prev_tests_passed) prev_tests_passed <- l_travis$passed_tests[k]}
      
    }
  }
  roster$count_1_test_passed[i] <- number_1_test
}

#find number of 1:1 commits (ie: student assed only 1 additional test over their previous number)
#over pa instead of student
#for each student and each PA need curr commit, prev commit, and see if there is a diff of 1
#only goes up to handle commits not working = 0 tests cases
pa_timestamps$s1_1_num_commits <- 0
pa_timestamps$s2_1_num_commits <- 0
for (i in 1:9) {
  #reset for every pa
  number_1_test <- 0
  for(j in 1:25) {
    #reset for every student in 01
    prev_tests_passed <- 0
    for (k in 1:l_travis_size) {
      #for all students, pa's, and commits, if a commit is a given students under a given pa
      if (l_travis$github_assignment_acceptor[k] == section1$githubusername[j]
          && l_travis$pa[k] == pa_timestamps$pas[i])
        #if 1 test is passed compared to the prev commit, add 1
      {(if (prev_tests_passed + 1 == l_travis$passed_tests[k]) number_1_test <- number_1_test + 1)
        #assign current commit passed_tests value to prev tests passed  
        if(l_travis$passed_tests[k] > prev_tests_passed) prev_tests_passed <- l_travis$passed_tests[k]}
      
    }
  }
  pa_timestamps$s1_1_num_commits[i] <- number_1_test
  
  #reset for every pa
  number_1_test <- 0
  for(j in 1:25) {
    #reset for every student in 02
    prev_tests_passed <- 0
    for (k in 1:l_travis_size) {
      #for all students, pa's, and commits, if a commit is a given students under a given pa
      if (l_travis$github_assignment_acceptor[k] == section2$githubusername[j]
          && l_travis$pa[k] == pa_timestamps$pas[i])
        #if 1 test is passed compared to the prev commit, add 1
      {(if (prev_tests_passed + 1 == l_travis$passed_tests[k]) number_1_test <- number_1_test + 1)
        #assign current commit passed_tests value to prev tests passed  
        if(l_travis$passed_tests[k] > prev_tests_passed) prev_tests_passed <- l_travis$passed_tests[k]}
      
    }
  }
  pa_timestamps$s2_1_num_commits[i] <- number_1_test
}



#separate data into section 1 and 2
section1 <- roster[1:25,]
section2 <- roster[26:50,]
s1_travis <- sqldf("SELECT l_travis.*, section1.userid FROM l_travis INNER JOIN section1 ON 
                         l_travis.github_assignment_acceptor = section1.githubusername")
s2_travis <- sqldf("SELECT l_travis.*, section2.userid FROM l_travis INNER JOIN section2 ON 
                         l_travis.github_assignment_acceptor = section2.githubusername")
s1_assignment <- sqldf("SELECT l_assignment.*, section1.githubusername FROM l_assignment INNER JOIN section1 ON 
                         l_assignment.studentid = section1.userid")
s2_assignment <- sqldf("SELECT l_assignment.*, section2.githubusername FROM l_assignment INNER JOIN section2 ON 
                         l_assignment.studentid = section2.userid")

#*****************************#
#Starting tests
#****************************#


#*****************************************
#address the question "did 02 students start assignments earlier? If so by how much?"
s1_creation <- repo_creation[repo_creation$section == 1,]
s1_creation <- as.numeric(unlist(matrix(s1_creation[6:11], nrow = 1)))
s2_creation <- repo_creation[repo_creation$section == 2,]
s2_creation <- as.numeric(unlist(matrix(s2_creation[6:11], nrow = 1)))

t.test(s2_creation, s1_creation, alternative="greater" )
sd(s2_creation)
sd(s1_creation)
#automatically bins data in 10 day period. if > 10 sticks in 10, otherwise sticks in corresponding
#area
breaks_start <- function(arr, interval = 1, max = 10) {
  num_breaks <- (max / interval) + 1
  value <- max - interval
  my_matrix <- matrix(0, nrow = 1, ncol = num_breaks)
  if (max %% interval != 0) print("error, max should be a multiple of interval")
  
  #for all values
  for (i in 1:(length(arr))) {
    #if value beats max, add to last row
    if(arr[i] >= max) my_matrix[num_breaks] <- my_matrix[num_breaks] + 1
    else {
      value <- max - interval
      curr_break <- num_breaks - 1
      while(value >= 0) {
        #if in certain zone, assign and break
        if(arr[i] >= value) {
          my_matrix[curr_break] <- my_matrix[curr_break] + 1
          break
        }
        value <- value - interval
        curr_break <- curr_break - 1
      }
    }
  }
  
  return(my_matrix)
}

#takes arr1 and arr2, checks if they have equal length or not. If one is shorter, adds "NA" to
#it until they're the same size. Then combines both into matrix with row labels of d1 and d2
combine_matrix <- function(arr1, arr2, d1, d2) {
  while(length(arr1) < length(arr2)) {
    arr1 <- matrix(c(arr1, NA), nrow = 1, byrow = TRUE)
  }
  while(length(arr2) < length(arr1)) {
    arr2 <- matrix(c(arr2, NA), nrow = 1, byrow = TRUE)
  }
  result <- matrix(c(arr1, arr2), 
                   byrow = TRUE,
                   nrow = 2,
                   dimnames = list(c(d1,d2), 1:length(arr1)))
  return(result)
}

s1_creation_breaks <- breaks_start(s1_creation, 1, 10)
s2_creation_breaks <- breaks_start(s2_creation, 1, 10)

time_started <- combine_matrix(s1_creation_breaks, s2_creation_breaks, "NLB", "LB")


barplot((time_started), 
        xlab = "Days before assignment was due",
        main = "Assignment start time (PA4-PA9)",
        ylab = "Number of repositories",
        beside = TRUE,
        width = 1,
        plot = TRUE,
        names.arg = c("0","1","2","3","4","5", "6", "7", "8", "9", "10+"),
        col = c("black", "white"))
legend("top", inset=.05, c("NLB","LB"), fill = c("black", "white")) 

#**********************************
#address the question "did 02 students finish assignments earlier?"
s1_time_finished <- s1_assignment[, "timeearly"]/3600/24
s2_time_finished <- s2_assignment[, "timeearly"]/3600/24
t.test(s2_time_finished, s1_time_finished, alternative="greater" )
sd(s2_time_finished)
sd(s1_time_finished)

s1_time_finished_breaks <- breaks_start(s1_time_finished, 1, 5)
s2_time_finished_breaks <- breaks_start(s2_time_finished, 1, 5)

time_finished <- combine_matrix(s1_time_finished_breaks, s2_time_finished_breaks, "NLB", "LB")


barplot((time_finished), 
        xlab = "Days before assignment was due",
        main = "Assignment completion time (PA3-PA9)",
        ylab = "Number of assignments",
        beside = TRUE,
        width = 1,
        plot = TRUE,
        names.arg = c("0","1","2","3","4", "5+"),
        col = c("black", "white"))
legend("top", inset=.05, c("NLB","LB"), fill = c("black", "white")) 



#*****************************
#address the question "did 02 students commit/push more?" 
# per repo currently

user_names <- rep(unlist(section1[, "githubusername"]), times = 7)
all_pa <- rep(unlist(pa_timestamps[3:9, "pas"]), times = 25)

#construct frame to hold data
s1_total_commits <- data.frame(user_names, all_pa, commits = 0)
#find every student and pa specific commit and store 
for(i in 1:nrow(s1_total_commits)) {
  num_commits <- 0
  for(j in 1:nrow(l_travis)) {
    if(s1_total_commits$user_names[i] == l_travis$github_assignment_acceptor[j]
       && s1_total_commits$all_pa[i] == l_travis$pa[j])
      num_commits <- num_commits + 1
  }
  s1_total_commits$commits[i] <- num_commits
}

user_names <- rep(unlist(section2[, "githubusername"]), times = 7)
s2_total_commits <- data.frame(user_names, all_pa, commits = 0)
for(i in 1:nrow(s2_total_commits)) {
  num_commits <- 0
  for(j in 1:nrow(l_travis)) {
    if(s2_total_commits$user_names[i] == l_travis$github_assignment_acceptor[j]
       && s2_total_commits$all_pa[i] == l_travis$pa[j])
      num_commits <- num_commits + 1
  }
  s2_total_commits$commits[i] <- num_commits
}
#run analysis
print(s1_total_commits <- unlist(s1_total_commits[, "commits"]))
print(s2_total_commits <- unlist(s2_total_commits[, "commits"]))
t.test(s2_total_commits, s1_total_commits, alternative="greater" )
sd(s2_total_commits)
sd(s1_total_commits)

#*****************************
#address the question "did 02 students commit/push more?" 
# BY STUDENT

user_names <- rep(unlist(section1[, "githubusername"]), times = 1)

#construct frame to hold data
s1_total_commits_travis <- data.frame(user_names, commits = 0)
#find every student and pa specific commit and store 
for(i in 1:nrow(s1_total_commits_travis)) {
  num_commits <- 0
  for(j in 1:nrow(s1_travis)) {
    if(s1_total_commits_travis$user_names[i] == s1_travis$github_assignment_acceptor[j]) {
      num_commits <- num_commits + 1
    }
  }
  s1_total_commits_travis$commits[i] <- num_commits
}

user_names <- rep(unlist(section2[, "githubusername"]), times = 1)
s2_total_commits_travis <- data.frame(user_names, commits = 0)
for(i in 1:nrow(s2_total_commits_travis)) {
  num_commits <- 0
  for(j in 1:nrow(s2_travis)) {
    if(s2_total_commits_travis$user_names[i] == s2_travis$github_assignment_acceptor[j]) {
      num_commits <- num_commits + 1
    }
  }
  s2_total_commits_travis$commits[i] <- num_commits
}
#run analysis
print(s1_total_commits_travis <- unlist(s1_total_commits_travis[, "commits"]))
print(s2_total_commits_travis <- unlist(s2_total_commits_travis[, "commits"]))
t.test(s2_total_commits_travis, s1_total_commits_travis, alternative="greater" )
sd(s2_total_commits_travis)
sd(s1_total_commits_travis)

#**********************************
# Total commits BY STUDENT OFF csv file
total_commmits <- read.csv("total_commits_df_gina.csv")
user_names <- rep(unlist(section1[, "githubusername"]), times = 1)

#construct frame to hold data
s1_total_commits <- data.frame(user_names, commits = 0)
#find every student and pa specific commit and store 
for(i in 1:nrow(s1_total_commits)) {
  num_commits <- 0
  for(j in 1:nrow(total_commmits)) {
    if(tolower(s1_total_commits$user_names[i]) == total_commmits$username[j])
      # For every pa add commits
      num_commits <- num_commits + total_commmits$PA3[j] + total_commmits$PA4[j] + total_commmits$PA5[j] + total_commmits$PA6[j] + total_commmits$PA7[j] + total_commmits$PA8[j] + total_commmits$PA9[j]
  }
  s1_total_commits$commits[i] <- num_commits
}

user_names <- rep(unlist(section2[, "githubusername"]), times = 1)
s2_total_commits <- data.frame(user_names, commits = 0)
for(i in 1:nrow(s2_total_commits)) {
  num_commits <- 0
  for(j in 1:nrow(total_commmits)) {
    if(tolower(s2_total_commits$user_names[i]) == total_commmits$username[j])
      # For every pa add commits
      num_commits <- num_commits + total_commmits$PA3[j] + total_commmits$PA4[j] + total_commmits$PA5[j] + total_commmits$PA6[j] + total_commmits$PA7[j] + total_commmits$PA8[j] + total_commmits$PA9[j]

  }
  s2_total_commits$commits[i] <- num_commits
}
#run analysis
print(s1_total_commits <- unlist(s1_total_commits[, "commits"]))
print(s2_total_commits <- unlist(s2_total_commits[, "commits"]))
t.test(s2_total_commits, s1_total_commits, alternative="greater" )
sd(s2_total_commits)
sd(s1_total_commits)

#get mean and sd of # commits over pas. No initial commits
for(i in 1:9) {
  #get numbers for s1
  num_commits <- 0
  for(j in 1:nrow(s1_travis)) {
    if (pa_timestamps$pas[i] == s1_travis$pa[j])
      num_commits <- num_commits + 1
  }
  pa_timestamps$s1_num_commits[i] <- num_commits
    
  #get numbers for s2
  num_commits <- 0
  for(j in 1:nrow(s2_travis)) {
    if (pa_timestamps$pas[i] == s2_travis$pa[j])
      num_commits <- num_commits + 1
  }
  pa_timestamps$s2_num_commits[i] <- num_commits
}
#run analysis
mean(pa_timestamps$s1_num_commits[3:9])
sd(pa_timestamps$s1_num_commits[3:9])
mean(pa_timestamps$s2_num_commits[3:9])
sd(pa_timestamps$s2_num_commits[3:9])

#graph commit data over pa
num_commits <- matrix(c(pa_timestamps[3:9, "s1_num_commits"],pa_timestamps[3:9, "s2_num_commits"]), 
                        nrow = 2, 
                        ncol = 7,
                        byrow = TRUE,
                        dimnames=list(c("NLB","LB"), toupper(pa_timestamps[3:9, "pas"])))
num_commits

p <- barplot(num_commits, 
             ylab = "commits",
             main = "Number of commits per PA",
             beside = TRUE,
             plot = TRUE,
             col = c("black", "white"))
legend("topleft", inset=.05, c("NLB","LB"), fill = c("black", "white")) 



#*****************************
#address the question "did 02 students have more of a 1:1 relationship
#with tests passed and commits/pushed?" Where 1:1 relationship is defined
#as the "Number of instances in which studdents only had a 1 test increase
#when they committed their code?"
s2_1_test_passed <- unlist(section2[, "count_1_test_passed"])
s1_1_test_passed <- unlist(section1[, "count_1_test_passed"])
t.test(s2_1_test_passed, s1_1_test_passed, alternative = "greater")
sd(s2_1_test_passed)
sd(s1_1_test_passed)

#graph # of 1:1 commits over pas
num_1_commits <- matrix(c(pa_timestamps[3:9, "s1_1_num_commits"],pa_timestamps[3:9, "s2_1_num_commits"]), 
                       nrow = 2, 
                       ncol = 7,
                       byrow = TRUE,
                       dimnames=list(c("NLB","LB"), toupper(pa_timestamps[3:9, "pas"])))
num_1_commits
barplot(num_1_commits, 
     ylab = "1:1 commits",
     main = "Number of times a student passed 1 additional test only",
     beside = TRUE,
     plot = TRUE,
     col = c("black", "white"))
legend("topleft", inset=.05, c("NLB","LB"), fill = c("black", "white")) 

#get per repo data
user_names <- rep(unlist(section1[, "githubusername"]), times = 7)
all_pa <- rep(unlist(pa_timestamps[3:9, "pas"]), times = 25)

#construct frame to hold data
s1_1_commits <- data.frame(user_names, all_pa, commits = 0)
#find every student and pa specific commit and store 
for(i in 1:nrow(s1_1_commits)) {
  number_1_test <- 0
  prev_tests_passed <- 0
  for(j in 1:nrow(l_travis)) {
    if(s1_1_commits$user_names[i] == l_travis$github_assignment_acceptor[j]
       && s1_1_commits$all_pa[i] == l_travis$pa[j])
      #if 1 test is passed compared to the prev commit, add 1
    {(if (prev_tests_passed + 1 == l_travis$passed_tests[j]) number_1_test <- number_1_test + 1)
      #assign current commit passed_tests value to prev tests passed  
      if(l_travis$passed_tests[j] > prev_tests_passed) prev_tests_passed <- l_travis$passed_tests[j]}
  }
  s1_1_commits$commits[i] <- number_1_test
}

user_names <- rep(unlist(section2[, "githubusername"]), times = 7)
s2_1_commits <- data.frame(user_names, all_pa, commits = 0)
#find every student and pa specific commit and store 
for(i in 1:nrow(s2_1_commits)) {
  number_1_test <- 0
  prev_tests_passed <- 0
  for(j in 1:nrow(l_travis)) {
    if(s2_1_commits$user_names[i] == l_travis$github_assignment_acceptor[j]
       && s2_1_commits$all_pa[i] == l_travis$pa[j])
      #if 1 test is passed compared to the prev commit, add 1
    {(if (prev_tests_passed + 1 == l_travis$passed_tests[j]) number_1_test <- number_1_test + 1)
      #assign current commit passed_tests value to prev tests passed  
      if(l_travis$passed_tests[j] > prev_tests_passed) prev_tests_passed <- l_travis$passed_tests[j]}
  }
  s2_1_commits$commits[i] <- number_1_test
}
mean(s1_1_commits[, "commits"])
sd(s1_1_commits[, "commits"])
mean(s2_1_commits[, "commits"])
sd(s2_1_commits[, "commits"])


#*************************
#address the question "what was the average total points per student 
#earned over the semester for 02? What would it have been for 01?"
s1_total_points <- section1[, "totalpoints"]
s2_total_points <- section2[, "totalpoints"]
t.test(s2_total_points, s1_total_points, alternative="greater" )
sd(s2_total_points)
sd(s1_total_points)

#per repo points
#get per repo data
user_names <- rep(unlist(section1[, "githubusername"]), times = 7)
all_pa <- rep(unlist(pa_timestamps[3:9, "pas"]), times = 25)

#construct frame to hold data
s1_points_repo <- data.frame(user_names, all_pa, points = 0)
#find every student and pa specific tests passed value and store
for(i in 1:nrow(s1_points_repo)) {
  num_points <- 0
  
  for(j in 1:nrow(s1_assignment)) {
    if(s1_points_repo$user_names[i] == s1_assignment$githubusername[j]
       && s1_points_repo$all_pa[i] == s1_assignment$modulename[j])
      num_points <- num_points + as.numeric(s1_assignment$pointsearned[j])
  }
  s1_points_repo$points[i] <- num_points
}

user_names <- rep(unlist(section2[, "githubusername"]), times = 7)
all_pa <- rep(unlist(pa_timestamps[3:9, "pas"]), times = 25)

#construct frame to hold data
s2_points_repo <- data.frame(user_names, all_pa, points = 0)
#find every student and pa specific tests passed value and store
for(i in 1:nrow(s2_points_repo)) {
  num_points <- 0
  
  for(j in 1:nrow(s2_assignment)) {
    if(s2_points_repo$user_names[i] == s2_assignment$githubusername[j]
       && s2_points_repo$all_pa[i] == s2_assignment$modulename[j])
      num_points <- num_points + as.numeric(s2_assignment$pointsearned[j])
  }
  s2_points_repo$points[i] <- num_points
}

mean(s1_points_repo[, "points"])
sd(s1_points_repo[, "points"])
mean(s2_points_repo[, "points"])
sd(s2_points_repo[, "points"])

#****************************
#address the question "did 02 students pass more tests as a whole?"
s1_total_tests <- section1[, "testspassed"]
s2_total_tests <- section2[, "testspassed"]
t.test(s2_total_tests, s1_total_tests, alternative="greater" )
sd(s2_total_tests)
sd(s1_total_tests)

#get # of tests over pas. 
pa_timestamps$s1_tests_passed <- 0
pa_timestamps$s1_tests_passed <- 0
for(i in 1:9) {
  #get numbers for s1
  num_tests <- 0
  for(j in 1:156) {
    if (pa_timestamps$pas[i] == s1_assignment$modulename[j])
      num_tests <- num_tests + as.numeric(s1_assignment$testspassed[j])
  }
  pa_timestamps$s1_tests_passed[i] <- num_tests / 25
  
  #get numbers for s2
  num_tests <- 0
  for(j in 1:170) {
    if (pa_timestamps$pas[i] == s2_assignment$modulename[j])
      num_tests <- num_tests + as.numeric(s2_assignment$testspassed[j])
  }
  pa_timestamps$s2_tests_passed[i] <- num_tests / 25
}
pa_timestamps$tests_passed_ave <- (pa_timestamps$s1_tests_passed + pa_timestamps$s2_tests_passed) / 2

#per repo testing
#get per repo data
user_names <- rep(unlist(section1[, "githubusername"]), times = 7)
all_pa <- rep(unlist(pa_timestamps[3:9, "pas"]), times = 25)

#construct frame to hold data
s1_tests_repo <- data.frame(user_names, all_pa, tests = 0)
#find every student and pa specific tests passed value and store
for(i in 1:nrow(s1_tests_repo)) {
  num_tests <- 0

  for(j in 1:nrow(s1_assignment)) {
    if(s1_tests_repo$user_names[i] == s1_assignment$githubusername[j]
       && s1_tests_repo$all_pa[i] == s1_assignment$modulename[j])
      num_tests <- num_tests + as.numeric(s1_assignment$testspassed[j])
  }
  s1_tests_repo$tests[i] <- num_tests
}

user_names <- rep(unlist(section2[, "githubusername"]), times = 7)
all_pa <- rep(unlist(pa_timestamps[3:9, "pas"]), times = 25)

#construct frame to hold data
s2_tests_repo <- data.frame(user_names, all_pa, tests = 0)
#find every student and pa specific tests passed value and store
for(i in 1:nrow(s2_tests_repo)) {
  num_tests <- 0
  
  for(j in 1:nrow(s2_assignment)) {
    if(s2_tests_repo$user_names[i] == s2_assignment$githubusername[j]
       && s2_tests_repo$all_pa[i] == s2_assignment$modulename[j])
      num_tests <- num_tests + as.numeric(s2_assignment$testspassed[j])
  }
  s2_tests_repo$tests[i] <- num_tests
}

mean(s1_tests_repo[, "tests"])
sd(s1_tests_repo[, "tests"])
mean(s2_tests_repo[, "tests"])
sd(s2_tests_repo[, "tests"])



#**************************
#address the question "were 02 students more active in Q and A forum?"
s1_total_responses <- section1[, "forumresponses"]
s2_total_responses <- section2[, "forumresponses"]
t.test(s2_total_responses, s1_total_responses, alternative="greater" )
sum(unlist(s2_total_responses))
sd(unlist(s2_total_responses))
sd(unlist(s1_total_responses))

#**************************
#github clone vs repo creation
#Was there a significant difference between repo creation and cloning
#**************************
#TODO take out intitial commit
first_clone_list <- as.numeric(unlist(matrix(repo_first_clone[4:11], nrow = 1)))
repo_creation_list <- as.numeric(unlist(matrix(repo_creation[4:11], nrow = 1)))
t.test(first_clone_list, repo_creation_list, alternative = "less")
sd(as.numeric(first_clone_list))
sd(repo_creation_list)
