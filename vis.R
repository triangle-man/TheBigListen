## Visualisation of The Big Listen survey data
## Experimental
## James Geddes
## April 2019

library(tidyverse)
library(GGally)
library(corrgram)

orig <- read_csv("data/survey2019.csv") %>%
    select(-c(`Completion time`, `Email`)) 

surv <- orig %>%
    gather(key = question, value = response, -`Start time`) %>%
    mutate(response = factor(response,
                             levels = c("Strongly agree",
                                        "Agree",
                                        "Neither agree nor disagree",
                                        "Disagree",
                                        "Strongly disagree"),
                             ordered = TRUE)) %>%
    mutate(response_score = as.numeric(response))

average_score <- surv %>%
    group_by(question) %>%
    summarise(score = mean(response_score)) %>%
    arrange(score)

## Bar chart of count of responses by question,
## ordered by average score for that question

fig_all <- 
    ggplot(data = surv %>%
                mutate(question = factor(question,
                                    levels = average_score$question,
                                    labels = str_wrap(average_score$question, 25),
                                    ordered = TRUE))) +
    geom_bar(aes(x = response_score)) +
    facet_wrap(~question) +
    labs(title = "All questions, ordered by average response") +
    theme(text = element_text(size = 7),
          strip.text = element_text(size = rel(0.7)))

pdf(file = "surv.pdf", paper="a4", height = 10)
fig_all
dev.off()


## Association plot

ggplot(data = surv) +
    geom_point(aes(response, response)) +
    facet_grid(question ~ question)

## Categories of question
## 1 - Mission and culture
## 2 - Strategy and management (change)
## 3 - Processes and operations (bau)
## 4 - Personal satisfaction

Qs <-
    tibble(question = c("I am confident my suggestions are fully considered",
                        "I have a good work life balance",
                        "This organisation has a strong conscience",
                        "I am proud to work for the Turing",
                        "I would recommend the Turing to my friends as an employer",
                        "My manager supports me and keeps me motivated" ,
                        "I receive regular constructive feedback" ,
                        "In my team, there is enough opportunity for people to discuss changes before they happen" ,
                        "The role of my team is well-understood by other departments" ,
                        "People in the Turing act as one team" ,
                        "People in the Turing take ownership and accept responsibility for outcomes" ,
                        "I’m confident in the direction the organisation is heading." ,
                        "People in the Turing look for flexible solutions to situations without compromising quality or the Turing’s ability to deliver" ,
                        "People in the Turing are always positive in their outlook" ,
                        "There are good opportunities for career development within the Turing" ,
                        "Across the Turing as a whole, there is enough opportunity for people to discuss changes before they happen" ,
                        "The Turing is effective at implementing necessary change" ,
                        "Senior managers provide effective leadership" ,
                        "Internal communication is good" ,
                        "Knowledge is shared effectively across the Turing" ,
                        "I feel like my personal development is important to the Turing" ,
                        "I value my performance review" ,
                        "I enjoy working for the Turing." ,
                        "The Turing benefits package is good" ,
                        "Performance is rewarded fairly at the Turing" ,
                        "I feel like I belong at the Turing" ,
                        "I understand the research ambitions and aims of the Turing." ,
                        "I feel confident about the future of the Turing." ,
                        "The Turing has a supportive environment" ,
                        "I think I can inform business processes." , "We work well across teams" ,
                        "I can explain the purpose of the Turing" ,
                        "The Turing has an inclusive culture" , "The Turing treats everyone fairly" ,
                        "I feel I impact on the success of the Turing" ,
                        "I feel my team impacts on the success of the Turing" ,
                        "I have the tools I need to be successful" ,
                        "I feel like I can be myself at the Turing" ,
                        "This job is good for my own personal growth" ,
                        "The physical work-space aids collaboration" ,
                        "The physical work-space meets my personal needs" ,
                        "I am happy with the pay and benefits I receive in this job"),
           q_type = c(4, ## "I am confident my suggestions are fully considered"                                                                            
                      4, ## "I have a good work life balance"                                                                                               
                      1, ## "This organisation has a strong conscience"                                                                                     
                      1, ## "I am proud to work for the Turing"                                                                                             
                      4, ## "I would recommend the Turing to my friends as an employer"                                                                     
                      2, ## "My manager supports me and keeps me motivated"                                                                                 
                      3, ## "I receive regular constructive feedback"                                                                                       
                      2, ## "In my team, there is enough opportunity for people to discuss changes before they happen"                                      
                      3, ## "The role of my team is well-understood by other departments"                                                                   
                      1, ## "People in the Turing act as one team"                                                                                          
                      1, ## "People in the Turing take ownership and accept responsibility for outcomes"                                                    
                      2, ## "I’m confident in the direction the organisation is heading."                                                                   
                      1, ## "People in the Turing look for flexible solutions to situations without compromising quality or the Turing’s ability to deliver"
                      1, ## "People in the Turing are always positive in their outlook"                                                                     
                      2, ## "There are good opportunities for career development within the Turing"                                                         
                      2, ## "Across the Turing as a whole, there is enough opportunity for people to discuss changes before they happen"                    
                      2, ## "The Turing is effective at implementing necessary change"                                                                      
                      2, ## "Senior managers provide effective leadership"                                                                                  
                      3, ## "Internal communication is good"                                                                                                
                      3, ## "Knowledge is shared effectively across the Turing"                                                                             
                      4, ## "I feel like my personal development is important to the Turing"                                                                
                      3, ## "I value my performance review"                                                                                                 
                      4, ## "I enjoy working for the Turing."                                                                                               
                      4, ## "The Turing benefits package is good"                                                                                           
                      3, ## "Performance is rewarded fairly at the Turing"                                                                                  
                      1, ## "I feel like I belong at the Turing"                                                                                            
                      2, ## "I understand the research ambitions and aims of the Turing."                                                                   
                      2, ## "I feel confident about the future of the Turing."                                                                              
                      1, ## "The Turing has a supportive environment"                                                                                       
                      3, ## "I think I can inform business processes."                                                                                      
                      3, ## "We work well across teams"                                                                                                     
                      1, ## "I can explain the purpose of the Turing"                                                                                       
                      1, ## "The Turing has an inclusive culture"                                                                                           
                      1, ## "The Turing treats everyone fairly"                                                                                             
                      4, ## "I feel I impact on the success of the Turing"                                                                                  
                      3, ## "I feel my team impacts on the success of the Turing"                                                                           
                      4, ## "I have the tools I need to be successful"                                                                                      
                      4, ## "I feel like I can be myself at the Turing"                                                                                     
                      4, ## "This job is good for my own personal growth"                                                                                   
                      3, ## "The physical work-space aids collaboration"                                                                                    
                      4, ## "The physical work-space meets my personal needs"                                                                               
                      4 ## "I am happy with the pay and benefits I receive in this job"
                      ),
           q_num = 1:42)
      
## Bar chart of count of responses by question,
## ordered by average score for that question,
## coloured by question type

ggplot(data = surv %>%
           left_join(Qs) %>%
           mutate(question = factor(question,
                                    levels = average_score$question,
                                    labels = str_wrap(average_score$question, 20),
                                    ordered = TRUE))) +
    geom_bar(aes(x = response_score, fill = as.character(q_type))) +
    facet_wrap(~question) +
    labs(title = "All questions, ordered by average response") +
    theme(text = element_text(size = 7),
          strip.text = element_text(size = rel(0.4)))
