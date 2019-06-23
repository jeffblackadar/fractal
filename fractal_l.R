library(R6)
library(ggplot2)
library(uuid)
options(stringsAsFactors = FALSE)
radian_conversion <- pi / 180

# from Adam Spannbauer OOP Fractal Trees in R with R6, ggplot2, & gganimate (part 1) https://adamspannbauer.github.io/2018/10/07/oop-fractal-trees-in-r-with-r6-ggplot2-gganimate-part-1/

line_base = R6Class(
  'line_base',
  public = list(
    start_x = NA_real_,
    start_y = NA_real_,
    end_x = NA_real_,
    end_y = NA_real_,
    type = NA_character_,
    id = NA_character_,
    line_color = NA_character_,

    
    initialize = function(x,
                          y,
                          len = 5,
                          theta = 90,
                          type = NA_character_,
                          line_color = '#000000') {
      dy = sin(theta * radian_conversion) * len
      dx = cos(theta * radian_conversion) * len
      
      self$start_x = x
      self$start_y = y
      self$end_x = x + dx
      self$end_y = y + dy
      
      self$type = type
      self$id = uuid::UUIDgenerate()
      self$line_color = line_color
    } #initialize
  ),
  # public
  
  active = list(
    df = function() {
      x = c(self$start_x, self$end_x)
      y = c(self$start_y, self$end_y)
      
      data.frame(
        x = x,
        y = y,
        type = self$type,
        id = self$id,
        line_color = self$line_color
      )
    }
  )  # active
)  # line_base



fractal_l_generate = R6Class(
  'fractal_l_generate',
  public = list(
    start_x = NA_real_,
    start_y = NA_real_,
    end_x = NA_real_,
    end_y = NA_real_,
    angle_delta = NA_real_,
    angle_current =  NA_real_,
    line_length = NA_character_,
    length_decay = NA_real_,
    min_length = NA_real_,
    fractal_lines = data.frame(),
    type = NA_character_,
    initiator = NA_character_,
    generators = list(),
    generator_number = NA_integer_,
    generation_current = NA_integer_,
    generation_max = NA_integer_,
    
    initialize = function(start_x = 0,
                          start_y = 0,
                          angle_delta = 0,
                          angle_current = 0,
                          line_length = 10,
                          length_decay = 1,
                          min_length = .25,
                          fractal_lines = data.frame(),
                          type = "",
                          initiator = "",
                          generators = c(""),
                          generator_number = 1,
                          generation_current = 0,
                          generation_max = 0) {
      self$start_x = start_x
      self$start_y = start_y
      self$angle_delta = angle_delta
      self$angle_current = angle_current
      self$line_length = line_length
      self$length_decay = length_decay
      self$min_length = min_length
      
      self$type = type
      self$initiator <- initiator
      self$generators <- generators
      self$generator_number <- generator_number
      self$generation_current <-
        generation_current
      self$generation_max <- generation_max
      
      
      fractal_instruction_line <- self$initiator
      if (self$generation_current == 0) {
        fractal_instruction_line <- self$initiator
      } else{
        fractal_instruction_line <- self$generators[self$generator_number]
      }
      
      generation_line_color <- "#000000"

      for (i in 1:nchar(fractal_instruction_line)) {
        fractal_instruction_char <- substr(fractal_instruction_line, i, i)
        
        if (fractal_instruction_char == 'F' |
            fractal_instruction_char == 'L' |
            fractal_instruction_char == 'R') {
          if (self$generation_current == self$generation_max) {
            new_line <-
              line_base$new(
                self$start_x,
                self$start_y,
                self$line_length,
                self$angle_current,
                self$type,
                generation_line_color
              )
            self$fractal_lines = rbind(self$fractal_lines, new_line$df)
            self$start_x <- new_line$end_x
            self$start_y <- new_line$end_y
          }
          else{
            #For F and L use generator_number = 1.  For R use generator_number = 2
            generator_number_temp <- 1
            if (fractal_instruction_char == 'R') {
              generator_number_temp <- 2
            }
            next_generation <-
              fractal_l_generate$new(
                start_x = self$start_x,
                start_y = self$start_y,
                angle_delta = self$angle_delta,
                angle_current = self$angle_current,
                line_length = self$line_length,
                length_decay = self$length_decay,
                min_length = self$min_length,
                type = self$type,
                initiator = self$initiator,
                generators = self$generators,
                generator_number = generator_number_temp,
                #F uses the first generator
                generation_current = self$generation_current + 1,
                generation_max = self$generation_max
              )
            
            self$fractal_lines = rbind(self$fractal_lines,
                                       next_generation$fractal_lines)
            
            self$angle_current <-
              next_generation$angle_current
            self$start_x <-
              next_generation$start_x
            self$start_y <-
              next_generation$start_y
          }
        }
        else{
          if (fractal_instruction_char == 'f') {
            if (self$generation_current == self$generation_max) {
              new_line <-
                line_base$new(
                  self$start_x,
                  self$start_y,
                  self$line_length,
                  self$angle_current,
                  self$type,
                  generation_line_color
                )
              self$start_x <- new_line$end_x
              self$start_y <- new_line$end_y
              
            }
            else{
              
              
              next_generation <-
                fractal_l_generate$new(
                  start_x = self$start_x,
                  start_y = self$start_y,
                  angle_delta = self$angle_delta,
                  angle_current = self$angle_current,
                  line_length = self$line_length,
                  length_decay = self$length_decay,
                  min_length = self$min_length,
                  type = self$type,
                  initiator = self$initiator,
                  generators = self$generators,
                  generator_number = 2,
                  #f uses the second generator
                  generation_current = self$generation_current + 1,
                  generation_max = self$generation_max
                )
              self$angle_current <-
                next_generation$angle_current
              self$start_x <-
                next_generation$start_x
              self$start_y <-
                next_generation$start_y
            }
          }
          else{
            if (fractal_instruction_char == 'T') {
              self$line_length <- self$line_length * length_decay
              if (self$line_length < self$min_length) {
                self$line_length <- self$min_length
              }
              generator_number_temp <- 1
              
              #trunk line
              new_line <-
                line_base$new(
                  self$start_x,
                  self$start_y,
                  self$line_length,
                  self$angle_current,
                  self$type,
                  generation_line_color
                )
              self$fractal_lines = rbind(self$fractal_lines, new_line$df)
              start_x_right <- new_line$end_x
              start_y_right <- new_line$end_y
              
              if (self$generation_current < self$generation_max) {
                next_generation <-
                  fractal_l_generate$new(
                    start_x = start_x_right,
                    start_y = start_y_right,
                    angle_delta = self$angle_delta,
                    angle_current = self$angle_current,
                    line_length = self$line_length,
                    length_decay = self$length_decay,
                    min_length = self$min_length,
                    type = self$type,
                    initiator = self$initiator,
                    generators = self$generators,
                    generator_number = generator_number_temp,
                    generation_current = self$generation_current + 1,
                    generation_max = self$generation_max
                  )
                
                self$fractal_lines = rbind(self$fractal_lines,
                                           next_generation$fractal_lines)
              }
            } # T
            
            else {
              if (fractal_instruction_char == 'B') {
                angle_right <- self$angle_current + self$angle_delta
                angle_left <- self$angle_current - self$angle_delta
                self$line_length = self$line_length * self$length_decay
                if (self$line_length < self$min_length) {
                  self$line_length = self$min_length
                }
                
                generator_number_temp <- 1
                #right line
                
                generation_line_color="green"
                if(self$generation_current>0 & self$generation_current<4){
                  generation_line_color="brown"
                }else{
                  if(self$generation_current>3 & self$generation_current<7){
                    generation_line_color="limegreen"
                  }else{
                    if(self$generation_current>6){
                      generation_line_color="green"
                    }else{
                      generation_line_color="pink"
                    } 
                  }
                  
                }
                generation_line_color
                new_line <-
                  line_base$new(
                    self$start_x,
                    self$start_y,
                    self$line_length,
                    angle_right,
                    self$type,
                    generation_line_color
                  )
                self$fractal_lines = rbind(self$fractal_lines, new_line$df)
                start_x_right <- new_line$end_x
                start_y_right <- new_line$end_y
                
                if (self$generation_current < self$generation_max) {
                  next_generation <-
                    fractal_l_generate$new(
                      start_x = start_x_right,
                      start_y = start_y_right,
                      angle_delta = self$angle_delta,
                      angle_current = angle_right,
                      line_length = self$line_length,
                      length_decay = self$length_decay,
                      min_length = self$min_length,
                      type = self$type,
                      initiator = self$initiator,
                      generators = self$generators,
                      generator_number = generator_number_temp,
                      generation_current = self$generation_current + 1,
                      generation_max = self$generation_max
                    )
                  
                  self$fractal_lines = rbind(self$fractal_lines,
                                             next_generation$fractal_lines)
                }
                
                #left line
                new_line <-
                  line_base$new(
                    self$start_x,
                    self$start_y,
                    self$line_length,
                    angle_left,
                    self$type,
                    generation_line_color
                  )
                self$fractal_lines = rbind(self$fractal_lines, new_line$df)
                start_x_left <- new_line$end_x
                start_y_left <- new_line$end_y
                
                if (self$generation_current < self$generation_max) {
                  next_generation <-
                    fractal_l_generate$new(
                      start_x = start_x_left,
                      start_y = start_y_left,
                      angle_delta = self$angle_delta,
                      angle_current = angle_left,
                      line_length = self$line_length,
                      length_decay = self$length_decay,
                      min_length = self$min_length,
                      type = self$type,
                      initiator = self$initiator,
                      generators = self$generators,
                      generator_number = generator_number_temp,
                      generation_current = self$generation_current + 1,
                      generation_max = self$generation_max
                    )
                  
                  self$fractal_lines = rbind(self$fractal_lines,
                                             next_generation$fractal_lines)
                }
                
              } else{
                if (fractal_instruction_char == '+') {
                  self$angle_current = self$angle_current + self$angle_delta
                }
                else{
                  if (fractal_instruction_char == '-') {
                    self$angle_current = self$angle_current - self$angle_delta
                  } else{
                    print("Unhandled instruction!")
                  } # else -
                }# else +
              } # else B
            } # else T
          } #else f
        } #else F
      } #for
    } # initialize
  )  # public
) # class


# Class fractal_l

fractal_l = R6Class(
  'fractal_l',
  public = list(
    start_x = NA_real_,
    start_y = NA_real_,
    angle_delta = NA_real_,
    angle_current =  NA_real_,
    line_length = NA_character_,
    length_decay = NA_real_,
    min_length = NA_real_,
    fractal_lines = data.frame(),
    type = NA_character_,
    initiator = NA_character_,
    generators = list(),
    generator_number = NA_integer_,
    generation_current = NA_integer_,
    generation_max = NA_integer_,
    fractal_definitions = data.frame(),
    
    initialize = function(line_length = 10,
                          angle_delta = pi / 4,
                          angle_current = 0,
                          initiator = "",
                          generators = c(""),
                          generator_number = 1,
                          length_decay = 0.7,
                          min_length = 0.25,
                          generation_max = 2) {
      
      
      self$line_length = line_length
      self$angle_delta = angle_delta
      self$angle_current = angle_current
      self$length_decay = length_decay
      self$min_length = min_length
       self$initiator <- initiator
      self$generators <- generators
      self$generator_number <- generator_number
      self$generation_max <- generation_max
      self$generation_current <- 0
      self$angle_delta <- angle_delta
      
      self$start_x <- 0
      self$start_y <- 0
      self$type <- "F"
      
      self$fractal_lines = fractal_l_generate$new(
        start_x = self$start_x,
        start_y = self$start_y,
        angle_delta = self$angle_delta,
        angle_current = self$angle_current,
        line_length = self$line_length,
        length_decay = self$length_decay,
        min_length = self$min_length,
        type = self$type,
        initiator = self$initiator,
        generators = self$generators,
        generator_number <- self$generator_number,
        generation_current = self$generation_current,
        generation_max = self$generation_max
      )$fractal_lines
    },
    plotf = function() {
      ggplot(self$fractal_lines, aes(x, y, group = id, color = line_color)) +
        geom_line() +
        scale_color_identity() +
        guides(color = FALSE, linetype = FALSE) +
        theme_void()
    }
  ) # public
)  # fractal_l


# Fractal instructions


l_names = c(
  'page 8',
  'page 9a',
  'page 9b',
  'page 9c',
  'page 10a',
  'page 10b',
  'page 10c',
  'page 10d',
  'page 10e',
  'page 10f',
  'page 11a',
  'page 11b',
  'page 12a',
  'page 12b',
  'page 2 Koch Snowflake',
  'Tree'
)
l_initiators = c(
  'F-F-F-F',
  'F-F-F-F',
  '-F',
  '-F+F+F+F',
  'F-F-F-F',
  'F-F-F-F',
  'F-F-F-F',
  'F-F-F-F',
  '-F-F-F-F',
  'F-F-F-F',
  '-L',
  '---R',
  '++L',
  '-R',
  'F++F++F',
  'T'
)
l_F_generators = c(
  'F-F+F+FF-F-F+F',
  'F+FF-FF-F-F+F+FF-F-F+F+FF+FF-F',
  'F+F-F-F+F',
  'F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF',
  'FF-F-F-F-F-F+F',
  'FF-F-F-F-FF',
  'FF-F+F-F-FF',
  'FF-F--F-F',
  'F-FF--F-F',
  'F-F+F-F-F',
  'L+R+',
  'R+L+R',
  'L+R++R-L--LL-R+',
  'LL-R-R+L+L-R-RL+R+LLR-L+R+LL+R-LR-R-L+L+RR-',
  'F-F++F-F',
  'B'
)
l_f_generators = c('',
                   '',
                   '',
                   'ffffff',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '-L-R',
                   'L-R-L',
                   '-L+RR++R+L--L-R',
                   'LL-R-R+L+LR+L-RR-L-R+LRR-L-RL+L+R-R-L+L+RR',
                   '',
                   '')
l_angles = c(90,
             90,
             90,
             90,
             90,
             90,
             90,
             90,
             90,
             90,
             90,
             60,
             60,
             90,
             60,
             17)

l_start_angles = c(90,
                   90,
                   90,
                   90,
                   90,
                   90,
                   90,
                   90,
                   90,
                   90,
                   90,
                   180,
                   90,
                   90,
                   0,
                   90)




l_systems <-
  data.frame(l_names,
             l_initiators,
             l_F_generators,
             l_f_generators,
             l_angles,
             l_start_angles)

plot_fractal_l <- function(l_systems_row_num, l_systems_max_generation){
  

  lSystem = fractal_l$new(
    line_length = 10,
    angle_delta = l_systems$l_angles[l_systems_row_num],
    angle_current = l_systems$l_start_angles[l_systems_row_num],
    initiator = l_systems$l_initiators[l_systems_row_num],
    generators = c(l_systems$l_F_generators[l_systems_row_num], l_systems$l_f_generators[l_systems_row_num]),
    generator_number = 1,
    length_decay = 0.7,
    min_length = 0.25,
    generation_max = l_systems_max_generation
  )
  lSystem$plotf()
}

