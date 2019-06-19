#load packages ----

pacman::p_load(caret,mlbench,ggplot2)


#load data ----

orders.dataset <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/orders_translated.csv')

line.item.dataset <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 6/Discover Associations Between Products/Data Sets/lineitems.csv')
