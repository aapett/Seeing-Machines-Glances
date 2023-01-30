library(apettr)
get_trips()

# Already Coded -----------------------------------------------------------
# coded annotation files
data_coded_files <- ezlist.files("C:\\Users\\apett\\Dropbox (AgeLab)\\Projects\\AVT_ST_Glance_Dataset_102020\\data\\Cadillac\\annotations_10Hz\\glance_annotations", "csv") %>%
  mutate(
    clip_id = str_extract(file, "\\d+_\\d+(?=_glance)"),
    trip_id = str_extract(clip_id, "^\\d+") %>% as.numeric()
  ) %>%
  left_join(trips %>% select(trip_id, trip_label_share, subject_id))

# source video files
video_coded_files <- ezlist.files("E:/avt/clips_for_analysis", "mp4$", T) %>%
  filter(
    str_detect(tolower(in_path), "ct6|cadillac"),
    !str_detect(tolower(in_path), "cadillac_ttc_baselines")
  ) %>%
  mutate(clip_id = str_extract(file, "\\d+_b?\\d+$") %>% str_remove("b")) %>%
  filter(clip_id %in% data_coded_files$clip_id) %>%
  group_by(clip_id) %>%
  mutate(n_vids = n()) %>%
  ungroup()

# filter coded to just those files which have 2-panel and 4-panel vids
# Not a requirement, just easier to work with
to_load <- data_coded_files %>%
  filter(clip_id %in% filter(video_coded_files, n_vids == 2)$clip_id)

# remove epochs more likely to be difficult to annotate
data_load <- pblapply(1:nrow(to_load), function(i) {
  data <- read_csv(to_load$in_path[i], col_types = "-c", progress = F) %>%
    distinct()
  if (any(data$label %in% c(
    "Eyes Closed",
    "No Eyes Visible",
    "not_annotatable",
    "not_annotated"
  ))) {
    return(NULL)
  }
  return(to_load[i, ])
}) %>% bind_rows()

# Random sample of 200 epochs, weighted by subject trips
data_random <- data_load %>%
  group_by(subject_id) %>%
  mutate(n = n()) %>%
  filter(n >= 12) %>%
  slice_sample(n = 12)

video_random <- video_coded_files %>%
  filter(clip_id %in% data_random$clip_id) %>%
  separate(clip_id, c("trip_id", "frame"), "_", F, T) %>%
  left_join(select(trips, trip_id, trip_label_share)) %>%
  mutate(
    panels = if_else(str_detect(in_path, "reclip"), 2, 4),
    out_path = paste0(
      "C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances/videos/",
      panels, "-panel/", trip_label_share, "_", frame, ".mp4"
    )
  ) %>%
  select(trip_label_share, clip_id, in_path, out_path)


# Copy Sample Files -------------------------------------------------------

# annotations 10Hz
# file.copy(data_random$in_path,"C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances/annotation/previous_10Hz")

# annotations 30Hz
ct6_videos <- ezlist.files("E:/avt/clips_for_analysis/ct6_tocs_glance", "b\\d+", T) %>%
  mutate(sample_id = str_extract(file, "\\d+_b\\d+$")) %>%
  distinct(sample_id)

v7_files <- ezlist.files("c:/users/apett/Dropbox (AgeLab)/Projects/v7_darwin_downloads/", "json", T) %>%
  mutate(sample_id = str_extract(file, "\\d+_b\\d+$")) %>%
  filter(sample_id %in% ct6_videos$sample_id) %>%
  mutate(
    start_frame = str_extract(file, "(?<=\\d{8}_\\d{4}_\\d{1,10}_[b_]?)-?\\d+|(?<=^\\d{1,10}_[b_]?)-?\\d+") %>% as.numeric(),
    sample_id = str_remove(sample_id, "b")
  ) %>%
  replace_na(list(start_frame = 0)) %>%
  filter(sample_id %in% sample_check$sample_id)

v7_data <- pblapply(1:nrow(v7_files), function(i) {
  data <- read_darwin_json(v7_files$in_path[i], v7_files$start_frame[i])
}) %>% bind_rows()

# There's a lot of non-cf data that isn't matched here, probably not a good solution
epoch_summary <- v7_data %>%
  mutate(
    sample_id = str_extract(filename, "\\d+_b?\\d+(?=\\.mp4)"),
    dataset = if_else(dataset == "steering control", "steeringcontrol", dataset)
  ) %>%
  group_by(dataset, sample_id) %>%
  summarise(
    annotation_type = str_extract(first(dataset), "glance|steeringcontrol|secondarytasks"),
    uncodable = all(label %in% c("No Eyes Visible", "not_codable")),
    framecount = n(),
    filename = first(filename)
  ) %>%
  pivot_wider(
    names_from = annotation_type,
    values_from = c(dataset, filename, uncodable, framecount),
    values_fn = ~ calculate_mode(.x, na.rm = T)
  )

good_epochs <- epoch_summary %>%
  separate(sample_id, c("trip_id", "frame"), "_b?", F, T) %>%
  left_join(trips %>% select(trip_id, trip_label_share))

data_glances <- good_epochs %>%
  select(filename = filename_glance, dataset = dataset_glance, sample_id) %>%
  left_join(v7_data)

pbwalk(1:nrow(good_epochs), function(i) {
  out_path <- paste0("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/annotation/previous_30Hz/", good_epochs$trip_label_share[i], "_", good_epochs$frame[i], "_glance.csv")
  data <- filter(data_glances, sample_id == good_epochs$sample_id[i]) %>%
    select(frame, label)
  write_csv(data, out_path, progress = F)
})

# videos
file.copy(video_random$in_path, video_random$out_path)

# can
can_random <- data_random %>%
  transmute(in_path = paste0(
    "C:\\Users\\apett\\Dropbox (AgeLab)\\Projects\\AVT_ST_Glance_Dataset_102020\\data\\Cadillac\\CAN_epochs_10Hz/",
    str_replace(file, "glance", "can"), ".csv"
  ))

file.copy(can_random$in_path, "C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances/can_10Hz")


# Already sampled ---------------------------------------------------------

# sampled already (Needed to replace anything less than 12 with new videos)
sample_check <- ezlist.files("C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances\\videos\\2-panel") %>%
  mutate(trip_label_share = str_remove(file, "_\\d+$")) %>%
  left_join(trips %>% select(trip_label_share, subject_id)) %>%
  group_by(subject_id) %>%
  mutate(n = n())

# #Use these as replacement clip_ids
# keepers <- c("305485_30302","302055_6482","302799_33638","302200_49907","303049_32372",
#              "304952_11246","302072_119387","304532_86798","305315_136406",
#              "425088_34832","523157_26828","523100_23330","523070_13229")

# make sure none of the dropped stuff is still in other folders
baddies <- ezlist.files("C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances", recursive = T) %>%
  mutate(file2 = str_remove(file, "_[:alpha:]+$")) %>%
  filter(file2 %not in% sample_check$file)

# clip out 4-panel videos
sample_vids <- ezlist.files("C:\\Users\\apett\\Dropbox (AgeLab)\\Sponsors\\seeing_machines\\seeing_machines_cadillac_glances\\videos\\4-panel")

clipping_4panels <- sample_check %>%
  filter(file %not in% sample_vids$file) %>%
  mutate(
    vid_path = paste0("E:/avt/subject_data/cadillac/vids/", trip_label_share, ".mp4"),
    start_frame = str_extract(file, "\\d+$") %>% as.numeric(),
    start_time = trunc_decimal((start_frame - 1) / 30),
    out_path = paste0("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/videos/4-panel/", file, ".mp4")
  )


pbwalk(1:nrow(clipping_4panels), function(i) {
  flags <- paste0("-loglevel panic -i ", add_quotes(clipping_4panels$vid_path[i]), " -ss ", clipping_4panels$start_time[i], " -t 20 -n ", add_quotes(clipping_4panels$out_path[i]))
  system2("ffmpeg", flags)
})

# Clip out 30Hz CAN
clipping_can <- sample_check %>%
  mutate(
    in_path = paste0("E:/avt/subject_data/cadillac/can/", trip_label_share, ".csv"),
    start_frame = str_extract(file, "\\d+$") %>% as.numeric(),
    end_frame = start_frame + 599,
    out_path = paste0("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/can/30Hz/", file, ".csv")
  )

pbwalk(1:nrow(clipping_can), function(i) {
  can <- read_csv(clipping_can$in_path[i],
    progress = F,
    col_types = cols_only(
      ts_micro = "d",
      vehicle_speed_mph = "d",
      steering_wheel_position_deg = "d",
      lateral_acceleration = "d",
      acc_active = "i",
      lkas_mode = "i"
    )
  ) %>%
    mutate(
      automation_level = case_when(
        acc_active == 0 & lkas_mode %in% c(0, 1) ~ "L0",
        acc_active == 1 & lkas_mode %in% c(0, 1) ~ "L1",
        T ~ "L2"
      ),
      frame = row_number() - 1
    ) %>%
    filter(between(frame, clipping_can$start_frame[i], clipping_can$end_frame[i])) %>%
    select(ts_micro, vehicle_speed_mph, steering_wheel_position_deg, automation_level, lateral_acceleration)

  write_csv(can, clipping_can$out_path[i], progress = F)
})

# Write New Annotations -------------------------------------------------------

library(apettr)
library(jsonlite)
get_trips()

sample_check <- ezlist.files("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/videos/2-panel") %>%
  mutate(trip_label_share = str_remove(file, "_\\d+$"),
         sample_id = str_extract(file,'\\d+_\\d+$')) %>%
  left_join(trips %>% select(trip_label_share, subject_id)) %>%
  group_by(subject_id) %>%
  mutate(n = n())

v7_files <- ezlist.files("c:/users/apett/Dropbox (AgeLab)/Projects/v7_darwin_downloads/cf67glanceseeingmachines", "json", T) %>%
  mutate(sample_id = str_extract(file,'\\d+_\\d+$')) %>%
  separate(sample_id,c("trip_id","start_frame"),"_",F,T)

v7_data <- pblapply(1:nrow(v7_files), function(i) {
  data <- read_darwin_json(v7_files$in_path[i], v7_files$start_frame[i])
}) %>% bind_rows()

#There's a lot of non-cf data that isn't matched here, probably not a good solution
epoch_summary <- v7_data %>%
  mutate(sample_id = str_extract(filename, "\\d+_b?\\d+(?=\\.mp4)" %>% str_remove("b")),
         dataset = if_else(dataset == "steering control", "steeringcontrol", dataset)) %>%
  group_by(dataset, sample_id) %>%
  summarise(
    annotation_type = str_extract(first(dataset), "glance|steeringcontrol|secondarytasks"),
    uncodable = all(label %in% c("No Eyes Visible", "not_codable")),
    framecount = n(),
    filename = first(filename) #temp undo
  ) %>%
  pivot_wider(names_from = annotation_type,
              values_from = c(dataset, filename, uncodable, framecount), #temp undo
              values_fn = ~calculate_mode(.x, na.rm = T))

good_epochs <- epoch_summary %>%
  filter(
    #if_all(starts_with("uncodable"), ~ . == F), #temp undo
    #framecount_glance == framecount_steeringcontrol
  ) %>%
  separate(sample_id,c("trip_id","frame"),"_b?",F,T) %>%
  left_join(trips %>% select(trip_id,trip_label_share))

data_glances <- good_epochs %>%
  select(filename = filename_glance, dataset = dataset_glance,sample_id) %>%
  left_join(v7_data) # %>% temp remove

pbwalk(1:nrow(good_epochs),function(i){
  out_path = paste0("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/annotation/new_30Hz/",good_epochs$trip_label_share[i],"_",good_epochs$frame[i],"_glance.csv")
  data <- filter(data_glances, sample_id == good_epochs$sample_id[i]) %>%
    select(frame,label)
  write_csv(data,out_path,progress = F)
})


# Data Match Check --------------------------------------------------------

files_can <- ezlist.files("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/can","csv") %>%
  rename(can_path = in_path)
files_annotation_old <- ezlist.files("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/annotation/archive/previous","csv") %>%
  mutate(file = str_remove(file,"_glance")) %>%
  rename(old_path = in_path)
files_annotation_new <- ezlist.files("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/annotation/archive/new","csv") %>%
  mutate(file = str_remove(file,"_glance")) %>%
  rename(new_path = in_path)
files_annotation_mediation <- ezlist.files("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/annotation/","csv") %>%
  mutate(file = str_remove(file,"_glance")) %>%
  rename(mediation_path = in_path)
files_toload <- left_join(files_can, files_annotation_old, "file") %>%
  left_join(files_annotation_new, "file") %>%
  left_join(files_annotation_mediation, "file")
rm(files_can, files_annotation_new, files_annotation_old, files_annotation_mediation)


data_load <- pblapply(1:nrow(files_toload),function(i){
  can <- read_csv(files_toload$can_path[i],col_types = "dddcd", progress = F) %>%
    mutate(frame = row_number() - 1 + (str_extract(files_toload$file[i],"\\d+$") %>% as.numeric()))
  old <- read_csv(files_toload$old_path[i],col_types = "dc", progress = F) %>%
    rename(old_label = label)
  new <- read_csv(files_toload$new_path[i],col_types = 'dc', progress = F) %>%
    rename(new_label = label)
  mediation <- read_csv(files_toload$mediation_path[i],col_types = 'dc', progress = F) %>%
    rename(mediation_label = label)

  joined <- full_join(can, old, "frame") %>%
    full_join(new,"frame") %>%
    full_join(mediation, "frame") %>%
    mutate(file = files_toload$file[i]) %>%
    relocate(file, frame) %>%
    mutate(bad_frame = is.na(ts_micro)|is.na(old_label)|is.na(new_label)|is.na(mediation_label),
           bad_match = old_label != new_label)

  return(joined)
}) %>% bind_rows()

status <- data_load %>%
  group_by(file) %>%
  summarise(n = n(),
            bad_frames = any(bad_frame),
            bad_matches = any(bad_match))

write_csv(
  data_load %>%
    group_by(file) %>%
    mutate(media_frame = row_number(),
           media_time = hms(trunc_decimal(media_frame/30))) %>%
    filter(bad_match) %>%
    select(file,frame,media_frame,media_time,old_label,new_label)
    ,
  "data_mediation.csv")


# Mediation replacement, writing, stats -----------------------------------

#mediated frames
data_mediated <- read_csv("data_mediation.csv") %>%
  mutate(across(c(old_label,new_label), list(match = ~ .x == mediated_label))) %>%
  select(file,frame,mediated_label,ends_with("match"))

#combine with coded data
data_w_mediation <- data_load %>%
  left_join(data_mediated, c("file", "frame")) %>%
  mutate(mediated_label = if_else(is.na(mediated_label), old_label, mediated_label)) %>%
  replace_na(list(old_label_match = T, new_label_match = T))

#quick stats
summary_w_mediation <- data_w_mediation %>%
  summarise(across(ends_with("match"),list(sum = sum, pct = ~ round(mean(.x) * 100,2))))

#write_mediation
pbwalk(unique(data_w_mediation$file),function(x){
  out_path = paste0("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/annotation/mediated/",x,"_glance.csv")
  data <- filter(data_w_mediation, file == x) %>%
    select(frame,label = mediated_label) %>%
    arrange(frame)
  write_csv(data,out_path,progress = F)
})

#quick file rename
# files <- ezlist.files("C:/Users/apett/Dropbox (AgeLab)/Sponsors/seeing_machines/seeing_machines_cadillac_glances/annotation", "csv", T) %>%
#   mutate(out_path = str_remove(in_path, "_glance(?=.csv$)"))
# file.rename(files$in_path,files$out_path)
