
Codebook for gesture coding
=============

## Columns that require hand-coding:

1.	**Order**<br>
	Ordinal numerical identifier within table per analysis
2.	**ID**<br>
	Unique numerical identifier within table
3.	**Archive_ID**<br>
	Identifier within the TV News Archive
4.	**Title**<br>
	Title within the TV News Archive
5.	**URL**<br>
	Permanent URL to TV News Archive
6.	**Snippet**<br>
	Transcription of speech in video
7.	**Phrase**<br>
	- low_standards: result of the search crawl for 'low standards'
	- high_standards: result of the search crawl for 'high standards'
	- lower_the standards: result of the search crawl for 'lower the standards'
	- raise_the_standards: result of the search crawl for 'raise the standards'

## Columns that do require hand-coding:

8.	**HasPhrase**<br>
	- yes: the relevant phrase ('low standards', 'high' standards') occurred
	- no: the relevant phrase ('high standards', 'low standards') did not occur
9.	**SpeakerVisible**<br>
	- yes: the speaker's face is visible for entirety of relevant phrase 
	- no: the speaker's face is not visible for entirety of relevant phrase 
10.	**HandsVisible**<br>
	- both: both hands are fully visible for entirety of relevant phrase
	- one: only one hand is fully visible for entirety of relevant phrase 
	- neither: neither hand is visible, or is partially occluded during at least part of relevant phrase
11.	**HandsVisibleGenerous**<br>
	- both: both hands are visible enough during relevant phrase to discern gesture from context 
	- one: only one hand is visible enough during relevant phrase to discern gesture from context
	- neither: neither hand is visible enough during relevant phrase to discern gesture from context
12.	**HandsFree**<br>
	- both: both hands are completely free
	- one: only one hand is completely free
	- neither: neither hand is completely free
13.	**HandsFreeGenerous**<br>
	- both: both hands are technically free but are unlikely to gesture
	- one: one hand is technically free but is unlikely to gesture
	- neither: neither hand is free
14.	**Aspect**<br>
	- only if Phrase == lower_the_standards, raise_the_standards
	- non-progressive: clause containing relevant phrase is not in the progressive grammatical aspect (e.g., 'we raised the standards')
	- progressive: clause containing relevant phrase  is in the progressive grammatical aspect (e.g., 'we **are raising** the standards')
	- none: clause containing relevant phrase does not contain aspect information (e.g., 'by raising the standards, we...'), relevant phrase is functioning as a gerund etc.
15.	**Agent**<br>
	- only if Phrase == lower_the_standards, raise_the_standards
	- speaker: speaker is agent of verb
	- other: someone or something else is agent of verb, or agent is unclear/omitted
16.	**Animacy**<br>
	- only if Agent == other
	- animate: speaker is referring to another human or living thing
	- inanimate: speaker is referring to an object or non-living thing (e.g., a bill being passed in the senate)
	- unclear: it is unclear who or what speaker is referring to 
17.	**Negated**<br>
	- no: relevant phrase is not negated (e.g., 'there is a high standard')
	- yes: relevant phrase is negated (e.g., 'there is **not** a high standard')
18.	**Contrasted**<br>
	- no: relevant phrase is standalone
	- yes: relevant phrase is contrasted with some reference to 'high', 'higher', 'low' or 'lower' standards (for phrases 'low standards', 'high standards'), or 'lowering/raising' standards (for phrases 'lower the standards', 'raise the standards') in local context
19.	**SpeakerName**<br>
	- name of speaker (if available)
20.	**Duplicate**<br>
	- no: video is not duplicated or video is first occurrence of a duplicated series in dataset
	- yes: video is a duplicate
21.	**NonNative**<br>
	- no: speaker is a native speaker of English
	- yes: speaker is not a native speaker of English
22.	**HandsMoving**<br>
	- yes: either hand is moving during relevant phrase
	- no: neither hand is moving during relevant phrase
23.	**NounPlural**<br>
	- only if HandsFreeGenerous == both
	- only if HandsMoving == yes
	- yes: noun 'standards' is plural
	- no: noun 'standard' is singular
24.	**ContextMove**<br>
	- only if HandsMoving == no
	- yes: speaker gestures before and/or after but not during relevant phrase
	- no: speaker does not gesture at all
25.	**WhichHand**<br>
	- L: left hand is moving
	- R: right hand is moving
	- both: both hands are moving
26.	**HandConfig**<br>
	- only if HandsMoving == yes
	- open: the gesturing hand is flat
	- halfway: the fingers of the gesturing hand make a right angle with palm
	- closed: the fingers of the gesturing hand are curled towards each other
27.	**PalmOrientation**<br>
	- only if HangConfig == open or halfway
	- up: palm facing upwards
	- down: palm facing downwards
	- inward: palm facing sideways, towards the centre of the body
	- front: palm facing camera
	- back: palm facing speaker
28.	**MovementVertical**<br>
	- up: hand specified ('WhichHand') is moving upwards
	- down: hand specified ('WhichHand') is moving downwards
29.	**MovementHorizontal**<br>
	- right: hand specified ('WhichHand') is moving rightwards
	- left: hand specified ('WhichHand') is moving leftwards
	- outward: right and left hands are moving away from each other
	- inward: right and left hands are moving towards each other
30.	**MovementSagittal**<br>
	- front: hand specified ('WhichHand') is moving frontally away from body
	- back: hand specified ('WhichHand') is moving frontally towards the body
31.	**Duration**<br>
	- duration of gesture, in milliseconds
32.	**StrokeDuration**<br>
	- duration of gesture stroke, in milliseconds
32.	**OfInterest**<br>
	- yes: particular video is worth discussing qualitatively in detail
	- no: particular video is not worth discussing qualitatively in detail
33.	**Comments**<br>
	- any comments, ideas or ambiguities encountered while coding

## Additional details for coding

1.	**Phrase**<br>
	- low_standards: includes 'low standards', 'low standard'
	- high_standards: includes 'high standards', 'high standard'
	- lower_the_standards: includes 'lower the standards', 'lowers the standard', 'lowers the standards', 'lower the standard', 'lowered the standards', 'lowered the standard', 'lowering the standards', 'lowering the standard'
	- raise_the_standards: includes 'raise the standards', 'raises the standard', 'raises the standards', 'raise the standard', 'raised the standard', 'raised the standards', 'raising the standard', 'raising the standards'
2.	**HasPhrase**<br>
	- if relevant phrase is interrupted by no more than one lexical item (e.g. ‘high **education** standards’), coded as 'yes'
	- if 'low/high' does not appear as part of noun phrase (e.g., ‘how **low** standards fall)’, coded as ‘no'
3. 	**SpeakerVisible**<br>
	- coded as 'yes' if speaker's nose, mouth and at least one eye are onscreen for whole of relevant phrase
	- coded as ‘no’ if audio is significantly out of sync with video during relevant phrase
	- coded as ‘no’ if gesturer’s speech is translated into English via voiceover
4.	**HandsVisible**<br>
	- coded as 'yes' if all speaker's fingers are visible
	- coded as 'no' if speaker is too far away from camera to discern gesture 
5.	**HandsFree**<br>
	The following instances coded as 'yes':
	- hands are free for some part but not all of relevant phrase
	- wrists but not hands are resting on table, lap or other surface
	- hands are holding a pen, or some other item that does not significantly restrict hand/finger movement
	The following instances coded as 'no':
	- hands are placed flat on table, lap or other surface
	- hands are gripping edge of table or podium
	- hands are holding a microphone, large book, notepad, piece of paper, or some other item larger than the speaker's hand  
	- hands are clasped together with fingers interlocked
	- hands are engaged in other task (e.g., shuffling papers or scratching head) 
	- arms are crossed
6.	**HandsFreeGenerous**<br>
	The following instances coded as 'yes':
	- hands are free for some part but not all of relevant phrase
	- wrists but not hands are resting on table, lap or other surface
	- hands are holding a pen, or some other item that does not significantly restrict hand/finger movement
	- hands are placed flat on table, lap or other surface
	- hands are gripping edge of table or podium
	The following instances coded as 'no':
	- hands are holding a microphone, large book, notepad, piece of paper, or some other item larger than the speaker's hand  
	- hands are engaged in other task (e.g., shuffling papers or scratching head) 
	- hands are clasped together with fingers interlocked
	- arms are crossed
7.	**Agent**<br>
	- if phrase appears in reported speech or where speaker is clearly taking the perspective of another speaker, coded as 'other'
	- use of 'we', even when used in a general sense (e.g., referring to the US as a whole), coded as 'speaker'
8.	**Animacy**<br>
	- if a pronoun is used in reported speech, or where speaker is clearly taking the perspective of another speaker, coded as 'animate'
	- if speaker uses generic 'you' pronoun, coded as 'animate'
	- if speaker refers to a group (e.g., the Republican Party) that can be conceptualised either as an abstract thing (i.e., the party itself) or as the people that comprise it, coded as 'unclear'
9.	**Negated**<br>
	- only clips where verb phrase is explicitly negated with 'not' coded as 'yes'
	- if 'not' negates another verb preceding 'raise' or 'lower' in verb phrase (e.g, 'we are **not trying** to lower the standards'), coded as 'no'
10.	**Contrasted**<br>
	The following criteria must be met for video to be coded as 'yes':
	- relevant phrase must be **intentionally** contrasted with another quantity (e.g., by contrasting 'raising' with 'lowering' in immediate context)
	- relevant phrase and contrast phrase must be uttered by same speaker 
11.	**NativeSpeaker**<br>
	- speakers born in countries where English is not official language coded as 'no'
	- speakers with foreign-sounding accents coded as 'no'
12.	**HandsMoving**<br>
	- video coded as ‘yes’ only if stroke of gesture co-occurs with relevant phrase
	- video coded as ‘no’ if movement is simply hands returning to rest position
13.	**ContextMove**<br>
	- if only one hand is visible at time of relevant phrase and does not gesture at all during video, but other non-visible hand becomes visible and **does** gesture, coded as 'no'


## Other instances

1.	**Video contains relevant phrase multiple times**<br>
	- if two speakers in video mention relevant phrase, first speaker’s utterance is analysed
	- if same speaker utters relevant phrase twice or more, first utterance of phrase is analysed
2.	**Speaker's gesture combines axes, uses different axes sequentially, or makes different movements using same axis sequentially **<br>
	- all axes movements noted in appropriate Movement columns
	- coding does not distinguish between combined axis movements and sequential different-axis movements 
	- sequential same-axis movements noted in order of movement, separated by forward slashes (/)
