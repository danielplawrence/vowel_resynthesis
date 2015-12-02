# # ## ### ##### ########  #############  ##################### 
# GUI-based wizard for creating realistic vowel formant continua 
# from modified natural speech
#
# version 30
#
# Matthew B. Winn
# April 2014
##################################
##################### 
############# 
######## 
#####
###
##
#
#
form Enter settings for Formant Continuum

   comment Formant and pitch analysis
	natural Enter_number_of_steps_in_the_formant_continuum 8
	natural Number_of_formants 5
	natural Maximum_formant_(Hz) 5000
	natural minimum_pitch_in_analysis_(Hz) 75
	natural maximum_pitch_in_analysis_(Hz) 250

   # select which formants you want to modify
	boolean modify_f1 0
	boolean modify_f2 1
	boolean modify_f3 1

   #override bandwidths?
	boolean Override_bandwidths 1	
   #Enter settings for frequency blending
	real Crossover_frequency_to_restore_original_signal_(Hz) 4000
	real Width_of_filter_crossover_(Hz) 500
		
   comment LPC resampling
   	optionmenu LPCchoice 3
	option Conventional_decimation
	option Variable_clean_decimation
	option Variable_decimation

   comment File settings
      	real Enter_final_intensity_for_all_new_sounds_(dB) 73

   #Enter directory path for the new files (a folder that already exists)
	sentence Enter_parent_directory_that_already_exists /Users/pplsuser/Desktop/Speech_Continua

   #Enter basic name for the folder of output files
	sentence Basic_name_for_new_folder_for_continuum_files Continuum_from_script

   #Enter prefix for the filenames (to be suffixed by continuum step number)
	sentence Enter_prefix_for_the_filenames Step_
endform

##############

call nameAdjustFromForm

call setVariables

call select2Sounds master reference

call selectManipulationPortions

call alterDurationAndOnsetOfRefSound

call selectSegment precursor
call selectSegment postcursor

call checkDuration precursor overlapDuration
call checkDuration postcursor overlapDuration

call recordDurations

call userAdjustPitchContour "'master$'" minpitch maxpitch

call extractLPportion "'master$'_PitchAdjusted" highpassCutoff skirt
call extractHPportion "'master$'_PitchAdjusted" highpassCutoff skirt

call makeSource "'master$'_PitchAdjusted" samplerate

call checkSource

call matchIntensityContour "'master$'_PitchAdjusted_SOURCE" 'master$'_PitchAdjusted_LP_portion 1 'blank$'

call makeFilter "'master$'" numformants maxformant windowLength
     if reference_choice != 2
	 call makeFilter "'reference$'_Matched" numformants maxformant windowLength
     endif

call cleanUpFormantGridPoints

call processBandwidths

call makePointVowels

call checkFormantGrid 'master$'

call checkReferenceGrid

call recheckFormantGrids

call formantGridToFormant "'master$'" no
call formantGridToFormant "'reference$'_Matched" no

call makeHeaderRows

call makeContinuumOfFormantGrids

call sourceFilterResynth

call makeSF_LPs

call matchLPintensities

call blendHPportions

call retainOriginalIntensityContours

call extractMiddleParts

call concatenateparts

call concatenateContinuum

call drawFormantTracks

call drawGradientSpectra

call printContinuumInfo

call initiateSave

call majorCleanup

call presentContinuum

#
#
##
###
#####
########
#############
#####################
################################## 
############################################ PROCEDURES 

procedure makePointVowels
    # uses the "whitened" voice source and makes a set of vowels that should be
    # easily recognizable as point vowels, and schwa. 
    # if the user hears the quality of the original vowel (from the master sound),
    # then the source "whitening" did not work well. 
	select FormantGrid 'master$'
	Copy... Blank
	for thisFormant from 1 to numformants
		call elideFormantGridPoints Blank 'thisFormant' 0 master_duration
		Remove bandwidth points between... 'thisFormant' 0 master_duration
		startManipulation_'master$'
	endfor

    # make the vowels
	#call makeVowel Blank 'master$_PitchAdjusted_SOURCE_adj_int Point_i 300 2300 2600 3500 70
	#call makeVowel Blank 'master$_PitchAdjusted_SOURCE_adj_int Point_a 800 1150 2400 3400 70
	#call makeVowel Blank 'master$_PitchAdjusted_SOURCE_adj_int Point_u 350 800 2300 3100 70
	#call makeVowel Blank 'master$_PitchAdjusted_SOURCE_adj_int Neutral_schwa 500 1500 2500 3500 70
	
	call makeVowel Blank 'master$_PitchAdjusted_SOURCE Point_i 290 2350 2600 3500 70
	call makeVowel Blank 'master$_PitchAdjusted_SOURCE Point_a 800 1150 2400 3400 70
	call makeVowel Blank 'master$_PitchAdjusted_SOURCE Point_u 350 800 2300 3100 70
	call makeVowel Blank 'master$_PitchAdjusted_SOURCE Neutral_schwa 500 1500 2500 3500 70

    # put the vowels together
	select Sound Point_i
	plus Sound Point_a
	plus Sound Point_u
	plus Sound Neutral_schwa
	Concatenate recoverably
	select Sound chain
	Rename... Point_vowels
	select TextGrid chain
	Rename... Point_vowels
	select Sound Point_vowels
	plus TextGrid Point_vowels
	View & Edit
	pause Listen to these vowels to ensure good resynthesis. 
endproc


procedure makeVowel .formantGrid$ .source$ .vowelName$ .f1 .f2 .f3 .f4 .intensity
	# same F5 for all vowel inputs
	.f5 = 4500
	select FormantGrid '.formantGrid$'
	Copy... '.vowelName$'
	# populate the new FormantGrid with formant values (input arguments)
	   for .thisFormant from 1 to numformants
		select FormantGrid '.vowelName$'
		Add formant point... '.thisFormant' 0 .f'.thisFormant'
		Add bandwidth point... '.thisFormant' 0.1 (60+(10*'.thisFormant'))
	   endfor
	# filter the source
	   select Sound 'master$'_PitchAdjusted_SOURCE
		plus FormantGrid '.vowelName$'
		Filter
		Rename... '.vowelName$'
	# add high-frequency energy (from the original master sound) to the vowel
		Formula... self [col] + Sound_'master$'_PitchAdjusted_HPportion [col]
	# zero out everything around the manip portion
		do ("Set part to zero...", 0, startManipulation_'master$', "at nearest zero crossing")
		do ("Set part to zero...", endManipulation_'master$', master_duration,"at nearest zero crossing")
		Scale intensity... '.intensity'
	# cleanup
		select FormantGrid '.vowelName$'
		Remove
endproc
	

procedure initiateSave
	beginPause ("Save files?")
	comment ("Do you want to save the new sound files & continuum info?")

    choice ("Save", 1)
       option ("Yes, save (on a PC)")
       option ("Yes, save (on a Mac) - not enabled yet")
       option ("No, do not save")
     endPause ("Cancel", "OK", 2)

	if save = 1
		mac = 0
		call adjustForMacOrPC
		call saveEverything
	elsif save = 2
		mac = 1
		call adjustForMacOrPC
		pause Saving on a Mac is not tested yet, so this might not work... 
		call saveEverything
	endif
endproc

procedure saveEverything
	call makeDirectories
	call saveInfoWindow "'parentDir$''spacer$''mainDir$'" 'outputFileName$'
	call saveSoundFiles
	call makeFileList "'parentDir$''spacer$''mainDir$''spacer$'Stimuli'spacer$'" 'listName$'
endproc
	
procedure saveSoundFiles
    # first, adjust numeric name of the continuum step to permit 
    # easy alphabeticization of one- and two-digit numbers
    # i.e. '2' becomes '02' so that 10 doesn't get ordered before 2. 
	for thisStep from 1 to 'formantSteps'
		if formantSteps > 9
		   if thisStep < 10
			tempStep$ = "0'thisStep'"
		   else   
			tempStep$ = "'thisStep'"
		   endif
		else
		   tempStep$ = "'thisStep'"
		endif
			select Sound 'basename$''thisStep'
			Rename... 'basename$''tempStep$'

	   call saveWavFile "'basename$''tempStep$'" 'parentDir$''spacer$''mainDir$''spacer$'Stimuli'spacer$'
	   call saveFormantGrid "FILTER_step_'thisStep'" 'parentDir$''spacer$''mainDir$''spacer$'FormantGrids'spacer$'
	endfor

	call saveWavFile "'reference$'" 'parentDir$''spacer$''mainDir$''spacer$'Original_sounds
	call saveWavFile "'master$'" 'parentDir$''spacer$''mainDir$''spacer$'Original_sounds

	call saveWavFile precursor 'parentDir$''spacer$''mainDir$''spacer$'Original_sounds
	call saveWavFile postcursor 'parentDir$''spacer$''mainDir$''spacer$'Original_sounds
endproc


procedure saveFormantGrid .name$ .directory$
	select FormantGrid '.name$'
	Save as text file... '.directory$''spacer$''.name$'.FormantGrid
endproc

procedure saveWavFile .name$ .directory$
	select Sound '.name$'
	Save as WAV file... '.directory$''spacer$''.name$'.wav
endproc	

procedure makeFileList .soundDir$ listName$
	Create Strings as file list... 'listName$' '.soundDir$'
	Save as raw text file... 'parentDir$''spacer$''mainDir$''spacer$''listName$'.txt
	select Strings 'listName$'
	Remove
endproc	


procedure saveInfoWindow outputDirectory$ outputFileName$
	filedelete 'outputDirectory$''spacer$''outputFileName$'.txt
	fappendinfo 'outputDirectory$''spacer$''outputFileName$'.txt
endproc


procedure makeDirectories
  # makes new directories - one as the main directory and one for the stimuli
	system_nocheck mkdir 'parentDir$''spacer$''mainDir$'
	system_nocheck mkdir 'parentDir$''spacer$''mainDir$''spacer$'Stimuli
	system_nocheck mkdir 'parentDir$''spacer$''mainDir$''spacer$'Original_sounds
	system_nocheck mkdir 'parentDir$''spacer$''mainDir$''spacer$'FormantGrids
endproc


procedure drawFormantTracks
   Erase all
   Select outer viewport... 0 6 0 5
   
   Line width... lineWidth
   for thisStep from 1 to 'formantSteps'
      # create a color gradient between blue & red, based on the step number
   	coolgradient = ('thisStep'-1)/('formantSteps'-1)
   		r = coolgradient
   		g = 0.0
   		b = 1-coolgradient
	Colour... {'r','g','b'}
   
   	select FormantGrid FILTER_step_'thisStep'
	Draw... (startManipulation_'master$'-0.015) (endManipulation_'master$'+0.015) 0 maxFormantDraw no yes lines
   endfor
   
   # Annotate the dotted lines (currently omitted because it creates a little clutter)
      #One mark bottom... startManipulation_'master$' no yes yes vowel start
      #One mark bottom... endManipulation_'master$' no yes yes vowel end
endproc

procedure drawGradientSpectra
	## draw gradient-colored smoothed spectra from each continuum step
	# Select the area beneath the formant tracks area
	do ("Select outer viewport...", 0, 6, 5, 9.5)
	
	for thisStep from 1 to formantSteps
	   # create a color gradient between blue & red, based on the step number
	   coolgradient = ('thisStep'-1)/('formantSteps'-1)
	   	# create rgb blend (starts at blue, ends at red)
		r = coolgradient
		g = 0.0
		b = 1-coolgradient
	   Colour... {'r','g','b'}

	   select Sound 'basename$''thisStep'
		name$ = selected$("Sound")
		tempDur = Get total duration
		# extract only the manipulated portion
		do ("Extract part...", precursorDuration, (tempDur - postcursorDuration), "rectangular", 1, "no")
		
		To Spectrum... yes
		Cepstral smoothing... 'smoothing'
		Rename... 'name$'_part_smooth
		select Spectrum 'name$'_part
		Remove
		select Spectrum 'name$'_part_smooth
		Draw... drawHzLow drawHzHigh drawDBLow drawDBHigh yes
		
		# cleanup
			select Spectrum 'name$'_part_smooth
			plus Sound 'name$'_part
			Remove
	endfor
	# re-select the formant tracks drawing
		Select outer viewport... 0 6 0 5
endproc

procedure concatenateContinuum
  # put them all into a single annotated sound so that they can be viewed & heard together
	select Sound 'basename$'1
	for thisStep from 2 to 'formantSteps'
	   plus Sound 'basename$''thisStep'
	endfor
	Concatenate recoverably
	
	select Sound chain
	Rename... 'basename$'Continuum
	select TextGrid chain
	Rename... 'basename$'Continuum
endproc


procedure presentContinuum
	select Sound 'basename$'Continuum
	plus TextGrid 'basename$'Continuum
	do ("View & Edit")
endproc

procedure majorCleanup
  # clean up all the remaining objects in the list
	select IntensityTier LPportion
	plus Sound precursor
	plus Sound postcursor
	for n from 1 to 'formantSteps'
	   plus Sound Step_'n'_SF
	   plus Sound Step_'n'_SF_with_HPportion
	   plus Sound Step_'n'_middle
	endfor
	if reference_choice=1
		#plus Manipulation 'reference$'
		#plus DurationTier 'reference$'
		plus Sound 'reference$'_Matched
		plus Formant 'reference$'_Matched
	endif
	plus Sound 'master$'_PitchAdjusted_HPportion
	plus Sound 'master$'_PitchAdjusted
	#plus Sound 'master$'_PitchAdjusted_SOURCE
	plus Formant 'master$'
	Remove
	
	select Sound 'master$'_PitchAdjusted_SOURCE
	Rename... 'master$'_Voice_Source
endproc

procedure selectManipulationPortions
	# always select landmarks for the master sound
	call selectManipulationPortion 'master$'

	   # select landmarks for the reference sound IF you chose a reference sound
	      if reference_choice != 2
			call selectManipulationPortion 'reference$'
		 else
		   	reference$ = "'master$'"
	      endif
	manipDuration = endManipulation_'master$' - startManipulation_'master$' 
endproc

procedure checkDuration .name$ .duration
  # ensure that a segment is at least as long as that required for temporal overlap. 
   select Sound '.name$'
   .tempdur = Get total duration
     if .tempdur <= (.duration/2)
	# sound must be expanded
	beginPause ("Duration")
	comment ("The duration of your '.name$' object is shorter than what is required")
	comment ("for cross-fading (per your overlapDuration.)")
	comment ("The segment must be greater than half the overlapDuration.")
	comment ("You can start over and adjust the overlapDuration in the script (bottom)")
	comment ("or choose one of the following options")

       choice ("alter_segment", 1)
          option ("Add extra silence to the '.name$' segment")
          option ("Shorten the overlap duration")

        endPause ("Cancel", "OK", 2)

	if alter_segment = 1
		targetDuration = overlapDuration+0.0001
		if .name$=="precursor"
		   # if it's the precursor, put the silence before the sound
		   call addOnsetSilence precursor targetDuration
		else
		   # if it's the postcursor, put the silence after the sound. 
		   call addOffsetSilence postcursor targetDuration
		endif
	else
		# increase the overlap duration to half the segment duration, minus a little bit
		overlapDuration = (.tempdur*2)-0.00001
	endif
   endif
endproc

procedure recordDurations
   # establish variables to be used later in the script
	select Sound precursor
		precursorDuration = Get total duration
	select Sound postcursor
		postcursorDuration = Get total duration
endproc

procedure addOffsetSilence .name$ .matchToThisDuration
	select Sound '.name$'
	.numChannels = Get number of channels
	Copy... temp
	select Sound '.name$'
	Remove
	do ("Create Sound from formula...", .name$, .numChannels, 0, .matchToThisDuration, samplerate, "0")
	formula... self [col] + Sound_temp [col]
	select Sound temp
	Remove
endproc
	
procedure addOnsetSilence .name$ .matchToThisDuration
	select Sound '.name$'
	.tempdur = Get total duration
	.numChannels = Get number of channels
	.durSilenceToAdd = .matchToThisDuration - .tempdur
	do ("Create Sound from formula...", "bufferSilence", .numChannels, 0, .durSilenceToAdd, samplerate, "0")
	select Sound '.name$'
	Copy... temp
	select Sound bufferSilence
	plus Sound temp
	Concatenate
	
	select Sound bufferSilence
	plus Sound temp
	plus Sound '.name$'
	Remove
	
	select Sound chain
	Rename... '.name$'
endproc

procedure concatenateparts
    select Sound postcursor
    Copy... temp_postcursor
   	for thisStep from 1 to 'formantSteps'
		select Sound precursor
		plus Sound Step_'thisStep'_middle
		plus Sound temp_postcursor
		if overlapDuration > 0
			# Concatenate with cross-fade
			Concatenate with overlap... overlapDuration
		else
		
			# If the overlap duration is 0, concatenate with no blending
			# if you accidentally set it to be a negative number, 
			#   it is coerced to zero here
			overlapDuration = 0
			Concatenate
		endif
		Rename... 'basename$''thisStep'
	
		Scale intensity... 'finalIntensity'
	endfor	
   select Sound temp_postcursor
   Remove
endproc

procedure extractMiddleParts
   # extract only the manipulation portion (user-defined region)
   # from the re-filtered sounds
   for thisStep from 1 to 'formantSteps'
	select Sound Step_'thisStep'_SF_with_HPportion
	Extract part... startManipulation_'master$' endManipulation_'master$' rectangular 1 no
	Rename... Step_'thisStep'_middle
   endfor
endproc


procedure retainOriginalIntensityContours
   # Each step in the continuum is matched to the intensity contour from the original master sound
   # This step occurs before the manipulated portion is boxed out,
   # because the user might use a different segment for the
   # leading / trailing segment. 
   for thisStep from 1 to 'formantSteps'
	call matchIntensityContour "Step_'thisStep'_SF_with_HPportion" 'master$' 1 'blank$'
   endfor
endproc


procedure blendHPportions
   # add the high-frequency portion from the original master sound
   # to the low-passed re-filtered continuum steps. 
   for thisStep from 1 to 'formantSteps'
	select Sound Step_'thisStep'_SF_LP
	Formula... self [col] + Sound_'master$'_PitchAdjusted_HPportion [col]
	Rename... Step_'thisStep'_SF_with_HPportion
   endfor
endproc


procedure matchLPintensities
   # Ensure that the manipulated portion of each continuum step
   # has an intensity contour that matches that of the 
   # corresponding frequency region in the original master sound. 
   for thisStep from 1 to 'formantSteps'
	select Sound Step_'thisStep'_SF_LP
	Scale intensity... 'original_LP_intensity'
   endfor
endproc


procedure makeSF_LPs
   # Take the re-filtered sound,
   # low-pass filter it so that only the frequency region chosen for manipulation
   # is present in the re-filtered signal. 
   # Higher-frequency portions are restored from the original signal
   # in a later procedure.
   for thisStep from 1 to 'formantSteps'
	select Sound Step_'thisStep'_SF
	Filter (pass Hann band)... 0 highpassCutoff skirt
	Rename... Step_'thisStep'_SF_LP
   endfor
endproc

procedure checkSource
   # check for "whiteness" of voice source spectrum. 
   # this step might be removed, as it is effectively replaced by the 
   # "makeVowels" procedure. 
   # However, this is the user's chance to filter the signal, 
   # in case they want to deliberately change the spectral slope,
   # or remove a spurious peak. 
	beginPause ("Check source")
	   comment ("For advanced users (optional): ")
	   comment ("Check the Source sound object for sound quality")
	   comment ("It should be devoid of any perceptible vowel quality.")
	   comment ("If you can clearly hear the original vowel, ")
	   comment ("consider switching which sound is the master / which is the reference")
	   endPause ("Cancel", "OK, I'm done", 2, 2)
endproc

procedure sourceFilterResynth
   # filter the voice source by each step in the formant continuum.
   for thisStep from 1 to 'formantSteps'
	select Sound 'master$'_PitchAdjusted_SOURCE
	plus FormantGrid FILTER_step_'thisStep'
	Filter
	Rename... Step_'thisStep'_SF
   endfor
endproc


procedure printFormants .formantObject$ .formant .step .start .end .timesteps
   # Print the formant values in the info windows
   print '.step''tab$''.formant''tab$'
   for thisTimeStep to .timesteps
	.timeStepSize = ('.end' - '.start')/('.timesteps'-1)
	.timepoint = (.timeStepSize*'thisTimeStep') + ('.start'-'.timeStepSize')

	# Get the formant value from the formant object
	select Formant '.formantObject$'
	.formantA = Get value at time... '.formant' '.timepoint' Hertz Linear

	# if the value is readable, print it
	if .formantA <> undefined
		print '.formantA:0''tab$'
	else 
	   # if it's undefined, print something as a placeholder. 
		print undefined'tab$'
	endif
   endfor
   print 'newline$'
endproc


procedure makeContinuumOfFormantGrids
   for thisFormantStep to 'formantSteps'
      # create the working filter object for this step
	select Formant 'master$'
	Down to FormantGrid
	Rename... FILTER_step_'thisFormantStep'

      # alter the formantgrid 
	  for thisFormant from 1 to numformants
	     # only alter it if it was selected to be altered
	     if f'thisFormant'mod = 1
	        # First delete all the existing formant points for that formant
		   call elideFormantGridPoints "FILTER_step_'thisFormantStep'" 'thisFormant' 0 master_duration
		
		# Next, insert the entire formant trajectory for that formant,
		# based on the interpolation between endpoints at this continuum step. 
		   call alterFormantGrid "FILTER_step_'thisFormantStep'" "'master$'" "'reference$'_Matched" thisFormant formantSteps thisFormantStep startManipulation_'master$' endManipulation_'master$' timesteps
	     else
		# if you aren't altering the formant, at least print out the formant values at the appropriate timepoints
		   call printFormants "'master$'" thisFormant thisFormantStep startManipulation_'master$' endManipulation_'master$' timesteps
	     endif
	endfor
   endfor
endproc


procedure alterFormantGrid .filter$ .master$ .reference$ .formant .numberOfSteps .thisStep .start .end .timesteps
   # Populate a formant row in a FormantGrid 
   # across the specified number of timepoints. 
   
   # First, delete all formant points in this formant row 
   	select FormantGrid '.filter$'
   	Remove formant points between... '.formant' .start .end
    
   # the formant number is the first column in the table of printed values
	print '.thisStep''tab$''.formant''tab$'

   for thisTimeStep to .timesteps
	## convert 'thisTimeStep' to an actual time value by interpolation
	.timeStepSize = ('.end' - '.start')/('.timesteps'-1)
	.timepoint = (.timeStepSize*('thisTimeStep'-1)) + '.start'
	
	# Get the formant value from the master sound
		select Formant '.master$'
		.formantA = Get value at time... '.formant' '.timepoint' Hertz Linear

	# Get the formant value from the reference sound
		select Formant '.reference$'
		.formantB = Get value at time... '.formant' '.timepoint' Hertz Linear

	# Proceed only if the formant at this timepoint is a valid number in BOTH 
	# the master & reference sound. 
	if .formantA <> undefined && .formantB <> undefined
	   if bark = 0
		## interpolate the formant value (linear) for this step at this timepoint
			.formantStep = ('.formantB' - '.formantA')/('.numberOfSteps' - 1)
			.formantInterp =  (.formantStep*('.thisStep'-1)) + .formantA
	   else
	   	## interpolate the formant value (Bark) for this step at this timepoint
	   		call freq2bark .formantA
	   			barkA = freq2bark.out
	   		call freq2bark .formantB
	   			barkB = freq2bark.out
	   		barkStep = (barkB-barkA)/('.numberOfSteps' - 1)
	   		barkInterp = (barkStep*(.thisStep-1))+barkA
	   		call bark2freq (barkInterp)
	   		# the formant value is the product of interpolation using the bark scale
	   		.formantInterp = bark2freq.out
	   endif
	   
	   # Add that formant value to the FormantGrid
			select FormantGrid '.filter$'
			Add formant point... '.formant' '.timepoint' '.formantInterp'
			
	       # Print out the formant value that you calculated,
	       # rounded to the nearest whole number
			print '.formantInterp:0''tab$'
	else
		# If the formants at this time point are not *both* readable 
		# in the master & reference sound, 
		# then print a placeholder
		print undefined'tab$'
		# and do not insert a point in the FormantGrid
	endif
   endfor
   print 'newline$'
endproc


procedure makeHeaderRows
   # Create a header row for the printed table for formant values
   # it will include two header rows: one with the time point, and one with the index of the time step. 
	print 'tab$'T-step 'tab$'
	
	# print timestep index
	for t from 1 to timesteps
		print 't''tab$'
	endfor
	print 'newline$'
	
	print Step'tab$'Formant'tab$'
	for thisTimeStep to timesteps
		timeStepSize = manipDuration/('timesteps'-1)
		timepoint = timeStepSize*('thisTimeStep'-1)
	
		print 'timepoint:3''tab$'
	endfor
	print 'newline$'
endproc


procedure formantGridToFormant .formantGrid$ .removeOriginal
   # Convert FormantGrid object to Formant object
   # So that you can query values from it. 
   # the second argument in the procedure is 
   # if you want to remove the original FormantGrid from the list
	select FormantGrid '.formantGrid$'
	To Formant... 0.01 0.1
	if .removeOriginal = 1
		select FormantGrid '.formantGrid$'
		Remove
	endif	
endproc	


procedure recheckFormantGrids
	pause (Optional) You can now take a moment to re-check any changes you made to the FormantGrids.
endproc


procedure checkFormantGrid .sound$
   # check & alter the FormantGrid and shape it into the contour that you want. 
   select FormantGrid '.sound$'
   View & Edit
	editor FormantGrid '.sound$'
	Set formant range... 0 maxformant
	Select... (startManipulation_'master$'-0.015) (endManipulation_'master$'+0.015)
	Zoom to selection

	
    # Pause window with some helpful tips
	beginPause ("Alter the formant tracks")
	comment ("Ensure that the formant tracks are smooth and continuous")
	comment ("Delete any spurious points by clicking them (or highlighting a section)")
	comment ("            and press ctrl+alt+T")
	comment ("Add a new formant point by pressing ctrl+T")
	comment ("Switch from one formant to another by pressing ctrl+(formant number)")
	comment ("            (e.g. ctrl+2 for F2)")
	comment ("   ")
	comment (" Note: if this is the master sound and you chose to *not* manipulate")
	comment ("    specific formants, then the contours you choose here will ")
	comment ("    be inherited by each continuum step.")
	comment ("   ")
	comment (" Click OK when you are finished altering the FormantGrid")
	endPause ("Cancel", "OK, I'm done", 2, 2)

	#Close
   endeditor
endproc

procedure checkReferenceGrid
   if reference_choice != 2
	call checkFormantGrid 'reference$'_Matched
   else
   	# If the user chose to simply make an alter-able copy of one sound,
   	# make a copy of that sound's FormantGrid,
   	# so that the user can work with two identical filters,
   	# and simply change the elements of interest. 

	   select FormantGrid 'master$'
	   Copy... 'reference$'_Matched
	# now offer the user a chance to change that into the new opposite endpoint
	   call checkFormantGridCopy 'reference$'_Matched
   endif
endproc

procedure checkFormantGridCopy .sound$
   select FormantGrid '.sound$'
   View & Edit
	editor FormantGrid '.sound$'
	Set formant range... 0 maxformant
	Select... (startManipulation_'master$'-0.015) (endManipulation_'master$'+0.015)
	Zoom to selection

    # pause window with helpful tips
	beginPause ("Alter the formant tracks")
	comment ("This is a copy of the FormantGrid that you just created")
	comment ("You can now design a new formant contour")
	comment ("for the opposite end of the continuum.")
	comment ("Delete any formant points by clicking them (or highlighting a section)")
	comment ("            and press ctrl+alt+T")
	comment ("Add a new formant point by pressing ctrl+T")
	comment ("Switch from one formant to another by pressing ctrl+(formant number)")
	comment ("            (e.g. ctrl+2 for F2)")
	comment ("   ")
	comment (" Click OK when you are finished altering the FormantGrid")
	comment (" Do not close the FormantGrid window.")
	endPause ("Cancel", "OK, I'm done", 2, 2)

	Close
   endeditor
endproc


procedure alterFormantGridBandwidths .sound$ .formant .endtime f1BW f2BW f3BW f4BW
   # Override bandwidth tracking by inserting static formant bandwidth values
	select FormantGrid '.sound$'
	Remove bandwidth points between... '.formant' 0 .endtime
	Add bandwidth point... .formant 0.1 f'.formant'BW
endproc


procedure cleanUpFormantGridPoints
   # Remove leading & trailing Formantgrid points
   # So that the user knows exactly when the manipulation portion
   # begins and ends. 
   # This is mostly aesthetic, as the portions affected by this step
   # are not included after the middle-portion extraction step. 
   for thisFormant from 1 to numformants
	call elideFormantGridPoints "'master$'" 'thisFormant' 0 startManipulation_'master$'
	call elideFormantGridPoints "'master$'" 'thisFormant' endManipulation_'master$' master_duration
		
	if reference_choice != 2
	   call elideFormantGridPoints "'reference$'_Matched" 'thisFormant' 0 startManipulation_'master$'
	   call elideFormantGridPoints "'reference$'_Matched" 'thisFormant' endManipulation_'master$' ref_lengthened_duration
	endif
   endfor
endproc

procedure processBandwidths
   # Check the user settings for formant bandwidht adjustment. 
   # if bandwidth override was chosen, execute that procedure. 
   if bandwidthOverride = 1
      for thisFormant from 1 to numformants
	call alterFormantGridBandwidths "'master$'" thisFormant master_duration f1BW f2BW f3BW f4BW
      endfor
   endif
endproc

procedure elideFormantGridPoints .formantGrid$ .formant .start .end
	select FormantGrid '.formantGrid$'
	Remove formant points between... '.formant' .start .end
endproc


procedure makeFilter .sound$ numformants maxformant windowLength
   # Make a formant object from the sound
   # Convert it into a FormantGrid, which the user can customize. 
   # The resulting FormantGrid will *only* be as good as the Formant object,
   # which is only as good as the settings used to generate it. 
   # Those settings are user-specified. 
	select Sound '.sound$'
	To Formant (burg)... 0 numformants maxformant windowLength 50
	select Formant '.sound$'
	Down to FormantGrid
	select Formant '.sound$'
	Remove
endproc


procedure matchIntensityContour .soundToAlter$ .referenceSound$ .removeOriginal .suffix$
   # Matches the overall intensity contour of one sound to that of another sound
   # The sounds are first aligned in the time domain. 
   # Procedure results in Sound object named '.soundToAlter$''.suffix$'
	select Sound '.soundToAlter$'
	.originalIntensity = Get intensity (dB)
	
	To Intensity... minpitch 0 yes
	Down to IntensityTier
	select Intensity '.soundToAlter$'
	mean = Get mean... 0 0 dB
	
	# future versions: avoid extreme values that throw off the intensity contour when flipped	
		# if you end up with a sound with undefined (/infinite) intensity
		# (you'll know because the spectrogram is all black),
		# then this is where it went wrong. 
		# If that occurs, create a pause after Copy... flipped
		# and visually inspect it to find dubious extreme values
		# that affect the mean/flipping conversion. 
		# Also, maybe check it *after* the flip as well, just in case. 
		# that will solve 99% of your problems. 
    # flip the intensitycontour around its mean
	select IntensityTier '.soundToAlter$'
	Copy... flipped
	Formula... -(self-mean)
	
    # multiply by the flipped contour to result in a FLAT contour
    # (a poor man's Hilbert transform)
	select Sound '.soundToAlter$'
	plus IntensityTier flipped
	Multiply... no
	Rename... '.soundToAlter$'_flattened

    # track the intensity contour of the reference sound
    # (the one after which the altered sound is modeled) 
	select Sound '.referenceSound$'
	To Intensity... minpitch 0 yes
	Down to IntensityTier

    #  Clean up original sound - need to do it here before the re-naming occurs
       if .removeOriginal = 1
    		select Sound '.soundToAlter$'
    		Remove
	endif

    # multiply the FLATTENED sound by the reference intensity contour
	select IntensityTier '.referenceSound$'
	plus Sound '.soundToAlter$'_flattened
	Multiply... no
	Rename... '.soundToAlter$''.suffix$'
	
    # Match it to the overall intensity so there isn't just an overall level change. 
	Scale intensity... '.originalIntensity'
	
    # cleanup remaining objects
	select Intensity '.soundToAlter$'
	plus IntensityTier '.soundToAlter$'
	plus IntensityTier flipped
	plus Sound '.soundToAlter$'_flattened
	plus Intensity '.referenceSound$'
	plus IntensityTier '.referenceSound$'
	Remove
endproc

procedure findIdealLPCResampleRate
	# returns variable ideal_LPC_resamplerate appropriate for LPC resampling
	# Prevents odd decimation by ensuring that the downsampling factor is a natural number. 
	# ensure that the sampling frequency is a whole number. 
	# this procedure ONLY gets executed if you choose "clean decimation"
	# as the LPC method, which is not recommended. 
	
	# establish variable
		nyquist = samplerate/2
	
	# ideally, we would like the LPC analysis range to be the same frequency range
	# used to track the formants. 
		idealResample = maxformant*2
		
	# Goal: establish new resampling frequency that is closest to,
	# but not less than the maximum sampling frequency needed to 
	# capture all of the desired formant peaks
	# (i.e. max formant freq. x2)
	# and below the Nyquist frequency

	# create a list of new potential sampling frequencies that are 
	# whole-number factors of the orginal rate
		# the loop goes to 10 but you probably only need it to go to 3-5
	for divisor from 1 to 10
		# each of these new variables is a numeric value that can be used as a sampling frquency
		samplerate_simple_factors_'divisor' = samplerate / 'divisor'
	endfor

		# print out all possible sampling frequencies 
		# (only implemented for debug mode)
		for i from 1 to 10
			# appendInfoLine(samplerate_simple_factors_'i')
		endfor
	# first initiate variable for the divisor of the sampling frequency
		thisDivisor = 1
	
	# increase the whole-number divisor until 
	# the downsampling rate is lower than the ideal resampling 
	# frequency defined by the formant settings
		while samplerate_simple_factors_'thisDivisor' > idealResample
		   thisDivisor = thisDivisor+1
		endwhile
	
	# After you've crossed that threshold, 
	# you have an index of which divisor crossed the threshold. 
	# Now get the *previous* index,
	# which produced a sampling frequency with a Nyquist frequency
	# that is higher than your maximum frequency for formant peaks,
	# (plus any extra bandwidth up to the new Nyquist freq.)
		idealDivisor = thisDivisor-1

	thisTempDivisor = idealDivisor
	# if the number rounded to zero does not equal the number,
	# choose a higher samplerate until it's a natural number
	
	if samplerate_simple_factors_'thisTempDivisor' mod 0 != samplerate_simple_factors_'thisTempDivisor'
		while samplerate_simple_factors_'thisTempDivisor' mod 0 != samplerate_simple_factors_'thisTempDivisor'
		   # then select the previous one - 
		   # a lower divisor, effectively raising the Nyquist higher
		   thisTempDivisor = thisTempDivisor -1
		endwhile
		idealDivisor = thisTempDivisor
	endif

	# establish the new rate by calling it from the list
	# previously made with the loop
		ideal_LPC_resamplerate = samplerate_simple_factors_'idealDivisor'
endproc

procedure makeSource .sound$ samplerate
	if lPCchoice = 1
		# conventional decimation
		lpcFreq = 11025
		numformants = 5

	elsif lPCchoice = 2
		# clean decimation
		cleanDecimate = 1
		call findIdealLPCResampleRate
		# get value from function
		lpcFreq = ideal_LPC_resamplerate
	
	elsif lPCchoice = 3
		variableDecimate = 1
			# "dirty" decimation - 
			# resample based on user formant settings 
			# this will yield the best tracking 
			# and best spectrum "whitening"
		lpcFreq = maxformant*2
	endif
	
	# first, anti-alias filter
		lpc_antiAlias_LPF = lpcFreq/2
		antiAlias_filterSkirt = 100
	
	# resample to prepare for LPC
		select Sound '.sound$'
		Resample... lpcFreq 50
			# yields Sound '.sound$'_'lpcFreq' with weird character if it's not a whole number
			# re-name to ensure whole number in Object name
		Rename... '.sound$'_'lpcFreq:0'
		target_resid_intensity = Get intensity (dB)
	
	# create LPC object
		To LPC (burg)... lpcOrder 0.025 0.005 50
			# yields LPC '.sound$'_'lpcFreq:0'

	# inverse filter the sound by the LPC to get the residual glottal source
		select Sound '.sound$'_'lpcFreq:0'
		plus LPC '.sound$'_'lpcFreq:0'
		Filter (inverse)
		Rename... '.sound$'_'lpcFreq'_reFilt

	# re-sample back up to the original sampling frequency
	# so that it can be combined with other sounds with the original sampling frequency
	# (The down-sampled object is never played as a wav file)
		Resample... samplerate 50
		Rename... '.sound$'_SOURCE
		Scale intensity... target_resid_intensity

	# cleanup
		select Sound '.sound$'_'lpcFreq:0'
		plus Sound '.sound$'_'lpcFreq'_reFilt
		plus LPC '.sound$'_'lpcFreq:0'
		Remove
endproc

procedure userAdjustPitchContour .sound$ .minpitch .maxpitch
   # lets the user adjust the pitch contour
   # "Ensure accurate pulses" means that for every pitch period, 
   # there should be a corresponding 
	select Sound '.sound$'
	To Manipulation... 0.01 minpitch maxpitch
	select Manipulation '.sound$'
	Edit
	editor Manipulation '.sound$'
		pause manipulate the Pitch contour to your liking. Ensure accurate pulses. Click Continue when finished
		#Close
	endeditor
	select Manipulation '.sound$'
	Get resynthesis (overlap-add)
	Rename... '.sound$'_PitchAdjusted
	
	select Manipulation '.sound$'
	Remove
endproc


procedure selectSegment .newname$
  # user is prompted to select a sound object from the list
  # that object is subsequently referred to in the script
  # by the string variable given as an input argument
  # simply to make the script more readable. 
	beginPause ("choose the '.newname$' to the manipulated sound")
	comment ("I want to select the segment from...")

    choice ("Choice", 1)
       option ("'master$' file")
       option ("'reference$' file")
       option ("Object in list")
       option ("Silence (see below)")
           positive ("Duration_of_silence", "0.05")
     endPause ("Cancel", "OK", 2)

 	if choice = 1
 		.sound$ = "'master$'"

 	elsif choice = 2
 		.sound$ = "'reference$'"
 	endif
 	
 	if choice <3
 		# user chose the master or reference (see above)
 		# open up the sound, mark landmarks for the segment
	 	   select Sound '.sound$'
 		   Edit
	 	   editor Sound '.sound$'
	 		pause Get start of the segment to become the '.newname$' THEN click Continue
			Move cursor to nearest zero crossing
	 		.start = Get cursor
			
	 		pause Get end of the '.newname$' segment THEN click Continue
			Move cursor to nearest zero crossing
	 		.end = Get cursor
			
			Select... '.start' '.end'
			Move start of selection to nearest zero crossing
			Move end of selection to nearest zero crossing
			Extract selected sound (time from 0)
			Close
		   endeditor
		   Rename... '.newname$'
		
		newname_file$ = selected$ ("Sound", 1)
		info'.newname$'$ = "A portion of '.sound$' was used as the '.newname$' file"
	endif	

	if choice = 3
		# user chose a sound object from the objects list
		# that complete sound will be renamed as postcursor or precursor
		# and resampled to match to the master sound. 
			pause Click on your '.newname$' sound file in the list, then click Continue. 
			newname_file$ = selected$ ("Sound", 1)
			info'.newname$'$ = "'newname_file$' was used as the '.newname$' file"
			Resample... 'samplerate' 50
			Rename... '.newname$'
	endif
	
	if choice = 4
	silenceDuration'.newname$' = duration_of_silence
	   .tempduration = silenceDuration'.newname$'
	   call makeSilence .tempduration 'samplerate' '.newname$'
	   	newname_file$ = selected$ ("Sound", 1)
		info'.newname$'$ = "'.tempduration' of silence was used as the '.newname$' file"
	endif
	
	select Sound '.newname$'
	duration'.newname$' = Get total duration
endproc


procedure alterDurationAndOnsetOfRefSound
   # only do this if there *is* a reference sound 
   if reference_choice != 2
     # get the duration ratio of of manipulation portions of master & reference sound
	masterManipDuration = endManipulation_'master$' - startManipulation_'master$'
	referenceManipDuration = endManipulation_'reference$' - startManipulation_'reference$'
	master_dur_ratio = masterManipDuration/referenceManipDuration
	master_onset_ratio = startManipulation_'master$'/startManipulation_'reference$'

     # Create a DurationTier that adjusts the manipulation portion by that ratio
	   select Sound 'reference$'
	   To Manipulation... 0.01 minpitch maxpitch
		Extract duration tier
		  Add point... (startManipulation_'reference$'-0.00001) master_onset_ratio
		  Add point... startManipulation_'reference$' master_dur_ratio
		  Add point... endManipulation_'reference$' master_dur_ratio
		  Add point... (endManipulation_'reference$'+0.00001) 1

	# Use that DurationTier to modify the reference sound 
	# to match timing landmarks to the master sound
		select Manipulation 'reference$'
		plus DurationTier 'reference$'
		Replace duration tier
	# Get the PSOLA modified version
		select Manipulation 'reference$'
		Get resynthesis (overlap-add)
		Rename... 'reference$'_Matched
		ref_lengthened_duration = Get total duration
	
	# cleanup  
		select Manipulation 'reference$'
		plus DurationTier 'reference$'
		Remove
endif
endproc

procedure selectManipulationPortion .sound$
    # select landmarks for onset & offset of manipulation,
    # record the time landmarks in the info window 
    # establish sound-specific variable names to record the values
	select Sound '.sound$'
	Edit
	editor Sound '.sound$'
	    pause Get start of the segment to manipulate, then click Continue
	 	Move cursor to nearest zero crossing
		startManipulation_'.sound$' = Get cursor
		temp = Get cursor
		print '.sound$' manipulation landmark (start) = 'temp''newline$'
	    pause Get end of the segment to manipulate, then click Continue
		Move cursor to nearest zero crossing
	 	endManipulation_'.sound$' = Get cursor
	 	temp = Get cursor
		print '.sound$' manipulation landmark (end) = 'temp''newline$'
	    Close
	endeditor
endproc	


procedure makeSilence .duration .samplerate .name$
	Create Sound from formula... '.name$' 1 0 .duration '.samplerate' 0
endproc

procedure extractLPportion .sound$ .cutoff .skirt
	select Sound '.sound$'
	Filter (pass Hann band)... 0 .cutoff .skirt
	original_LP_intensity = Get intensity (dB)
	To Intensity... minpitch 0 yes
	Down to IntensityTier
	Rename... LPportion
	select Intensity '.sound$'_band
	Remove
	select Sound '.sound$'_band
	Rename... '.sound$'_LP_portion

endproc

procedure extractHPportion .sound$ .cutoff .skirt
	## create high-pass portion of the sound 
	select Sound '.sound$'
		Filter (stop Hann band)... 0 .cutoff .skirt
		Rename... '.sound$'_HPportion
endproc	


procedure select2Sounds .label1$ .label2$
	pause Select the '.label1$' sound
	'.label1$'$ = selected$ ("Sound")
	samplerate = Get sampling frequency
	numchannels = Get number of channels
	master_duration = Get total duration
	
    # if sound is stereo, convert to mono
     if numchannels = 2
	   beginPause ("Warning about mono conversion ")
		comment ("The sound that you selected is stereo")
		comment ("This script cannot preserve dichotic channel differences")
		comment ("so it is now a mono sound")
	    endPause ("Cancel", "OK", 2)

	   select Sound 'master$'
	   Convert to mono
	   Rename... 'master$'_mono
     # re-establish variables
   	select Sound 'master$'_mono
   	'.label1$'$ = selected$ ("Sound")
	samplerate = Get sampling frequency
	numchannels = Get number of channels
	master_duration = Get total duration
     endif
	
   beginPause ("Choose the second sound ")
	comment ("Choose the second (reference) sound to be used for analysis")
	comment ("Either select a new sound in the Objects list now to serve as the referent,")
	comment ("or just work with the sound you already selected, with no referent")

    choice ("reference_choice", 1)
       option ("A new object from the list")
       option ("No new sound - just modify the original master sound")    
     endPause ("Cancel", "OK", 2)

 	if reference_choice = 1
 		pause Select the '.label2$' sound
		   '.label2$'$ = selected$ ("Sound")
	else
		#print Only one sound used for manipulation: 'master$' 'newline$'
	endif
endproc


procedure adjustForMacOrPC
# adjusts filepath settings for PC/Mac
	if mac = 1
		spacer$ = "/"
	else
		spacer$ = "\"
	endif
endproc

procedure printContinuumInfo
	print 'newline$''newline$'
	print Continuum script setup information'newline$'
	print 'enter_number_of_steps_in_the_formant_continuum''tab$'steps in the continuum'newline$'
	print 'number_of_formants''tab$'formants analyzed'newline$'
	print 'maximum_formant''tab$'maximum frequency for formant analysis'newline$'
	print 'minimum_pitch_in_analysis''tab$'minimum pitch for analysis'newline$'
	print 'maximum_pitch_in_analysis''tab$'maximum pitch for analysis'newline$'

	print 'newline$'
	print Which formants were modified: (1 is yes, 0 is no)'newline$'
	print    F1: 'f1mod''newline$'
	print    F2: 'f2mod''newline$'
	print    F3: 'f3mod''newline$'
	print    F4: 'f4mod''newline$'

	print 'newline$'
	print    Bandwidth override: 'bandwidthOverride''newline$'
		if bandwidthOverride = 1
		   print      F1BW = 'f1BW''newline$'
		   print      F2BW = 'f2BW''newline$'
		   print      F3BW = 'f3BW''newline$'
		   print      F4BW = 'f4BW''newline$'
		   print 'newline$'
		endif

	print 'newline$'
	print 'crossover_frequency_to_restore_original_signal''tab$'crossover frequency to restore original signal'newline$'
	print 'width_of_filter_crossover''tab$'width of crossover filter'newline$'
	print 'enter_final_intensity_for_all_new_sounds''tab$'final intensity for new sounds'newline$'
	print 'newline$'
	print parent directory:'tab$''enter_parent_directory_that_already_exists$''newline$'
	print sub-directory:'tab$''tab$''basic_name_for_new_folder_for_continuum_files$''newline$'
	print filename prefix:'tab$''enter_prefix_for_the_filenames$''newline$'

	print 'newline$'
	
	print 'infoprecursor$''newline$'
	print 'infopostcursor$''newline$''newline$'
	
	temp = startManipulation_'master$'
	print Start of manipulation (master sound): 'temp''newline$'
	
	temp = endManipulation_'master$'
	print End of manipulation (master sound): 'temp''newline$'
	
	if reference_choice = 2
			print Only one sound used for manipulation: 'master$' 'newline$'
	else
		temp = startManipulation_'reference$'
		print Start of manipulation (reference sound): 'temp''newline$'

		temp = endManipulation_'reference$'
		print End of manipulation (reference sound): 'temp''newline$'
	endif
	
	print 'newline$'
	print Original sampling fequency: 'samplerate''newline$'
	print 'newline$'
	print LPC order: 'lpcOrder''newline$'
	print LPC resample rate: 'lpcFreq''newline$'
		decimationFactor = samplerate/lpcFreq
	print LPC decimation factor: 'decimationFactor''newline$'
endproc


## uses the Hz to Bark conversion from Traunmuller (1990); 
	## Hz to Bark: (26.81/(1+(1960/f))) - 0.53
	## Bark to Hz: f = 1960 / [26.81 / (z + 0.53) - 1]
	
procedure bark2freq .bark
	.out = 1960 / (26.81 / ('.bark' + 0.53) - 1)
endproc

procedure freq2bark .freq
	.out = (26.81/(1+(1960/'.freq'))) - 0.53
endproc

procedure nameAdjustFromForm
     # variable name adjustment from the input settings form 
     # (the form name was formatted to be easily readable,
     # and the values are assigned to variable names
     # that are more script-friendly. 

	formantSteps = enter_number_of_steps_in_the_formant_continuum
	numformants = number_of_formants
	maxformant = maximum_formant
	minpitch = minimum_pitch_in_analysis
	maxpitch = maximum_pitch_in_analysis

	f1mod = modify_f1
	f2mod = modify_f2
	f3mod = modify_f3

	bandwidthOverride = override_bandwidths

	highpassCutoff = crossover_frequency_to_restore_original_signal

	skirt = width_of_filter_crossover

	finalIntensity = enter_final_intensity_for_all_new_sounds

	basename$ = enter_prefix_for_the_filenames$

	parentDir$ = enter_parent_directory_that_already_exists$

	mainDir$ = basic_name_for_new_folder_for_continuum_files$
	
	# sets output (Continuum info) file name using the basename from the folder
		outputFileName$ = "'mainDir$'Continuum_Info"

	# sets name for the file list using the basename from the folder
		listName$ = "'mainDir$'file_list"

	#initialize variables - omitted now (now in-line in the script)
		conventionalDecimate = 0
		cleanDecimate = 0
		variableDecimate = 0

	# activate lpc sampling choice based on option menu
	if lPCchoice = 1
		conventionalDecimate = 1
	elsif lPCchoice = 2
		cleanDecimate = 1
	elsif lPCchoice = 3
		variableDecimate = 1
	endif
endproc


#########################################
#########################################

procedure setVariables
	## set some variables for the script. 
	## The user should adjust these as desired
	
	clearinfo

	f1BW = 75
	f2BW = 85
	f3BW = 90
	f4BW = 100
	f5BW = 210

	# number of timepoints to extract vowel information. Larger number means more accuracy for trajectories. 
	   # this controls how many timepoints are referenced from the formant tracks 
	   # when you create the new stimuli.  
	   # Values are interpolated between points
	   # NOTE: if you want steady-state vowels, it is recommended
	   # that you do not set this value to be 1, but rather
	   # Adjust the formant tracks by hand to reflect the contour that you want.
	   # Also, you probably want to maintain the onset & offset transitions 
	   # out from and into the adjacent consonant sounds.
	   # for long vowels (including vowels in open syllables),
	   # you probably want this number to be 25 or above
	   # in order to properly track consonant transitions.
	   # if you're specifically interested in consonant transitions, the more the better.
	timesteps = 30

	# window length for formant analysis
		windowLength = 0.04

	# basic boolean variables
		yes = 1
		no = 0

	# basic blank text placehodler
		blank$ = ""

	# use bark interpolation instead of linear interpolation of formant frequencies
	# set to 0 if you want linear interpolation
		bark = 1

	# don't alter the fourth formant
	   # set to 1 if you want F4 to vary
	   # if you don't track F4, this value wont matter. 
	   # If F4 is above the frequency cutoff above which you blend in
	   # energy from the original signal, this value wont matter. 
		modify_f4 = 0
		modify_f5 = 0
	
	# convert variable name from input window
		f1mod = modify_f1
		f2mod = modify_f2
		f3mod = modify_f3
		f4mod = modify_f4
		f5mod = modify_f5
	
	# if you chose to modify F3 but are not tracking F3, alert the user
	   if numformants < 3
		f3mod = 0 
		pause F3 will not be manipulated because it is not being tracked (check input formant settings)
	   endif

	if highpassCutoff > maxformant
		highpassCutoff = maxformant
		beginPause ("Crossover filter adjusted")
		comment ("Crossover filter adjusted downward because of formant settings. ")
		comment ("Crossover frequency must be higher than the max frequency for formant analysis")
		comment ("to ensure a full spectrum.")
		endPause ("Cancel", "OK", 2, 2)
	endif

	# drawing formant tracks after the script is complete
		maxFormantDraw = 3000
		lineWidth = 2

	# drawing spectra after the script is complete
		smoothing = 300
		drawHzLow = 0
		drawHzHigh = 6000
		drawDBLow = -15
		drawDBHigh = 45
	
	# alter LPC parameters based on the user's input formant analysis settings
	# LPC order = two poles for each formant in the freq range, 
	# plus two, to add two poles for sound source (spectral tilt)
		lpcOrder = (numformants*2)+2
		
		# if conventional LPC analysis, it is almost always the case that you want at least 10 poles
		### if you want to change it (as for chidrens' voices 
		# or other difficult voices), change it here
			if lpcOrder < 10 
			   if conventionalDecimate = 1
			      lpcOrder = 10
			   endif
			endif

	
	# set overlap duration between segment onset, vowel and offset
	  # HALF of this duration will be the length of the cross-fade time for each part
	  # if you set this to zero, then it's a straight concatenation 
	  # the larger this value, the smoother the transition between the segments,
	  # but you lose some info in each segment (because more of it is faded out)
		overlapDuration = 0.003
	
endproc