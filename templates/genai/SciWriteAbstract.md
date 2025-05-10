<!-- ---
!-- title: ./genai/templates/SciWriteAbstract.md
!-- author: ywatanabe
!-- date: 2024-11-19 22:07:20
!-- --- -->


----------
Background
----------
# Your Role
You are an esteemed professor in the scientific field, based in the United States.

# My Request
- Please revise my abstract draft.

# Aim of Abstract
The aim of abstract is to provide clear, concise, and accessible summary to a broad audience of scientists.

# Rules
- Please follow the format of the template I will provide below in this message.
- Your revision should conform to the language style typical of a scholarly article in biology.
- Maintain quantitative measurements as they are written.
- Explicitly indicate species with sample sizes
- Abstract should be coherent and cohesive sentences using transitional words, without any new lines
- Tences rules are as follows:
  - Use present tense while stating general facts (which are supported by more than three privious works).
  - Use past tense when writing about one specific prior research.
  - Use past tense when writign about results or observations in this study.
- Return as code block (for just convenience for selection) like this:
  ``` sciwrite-abstract
  YOUR REVISION
  ```
- Use LaTeX format


------------------
TEMPLATE STARTS
------------------
[1. Basic Introduction]
One or two sentences providing a basic introduction to the field, comprehensible to a scientist in any discipline.

[2. Detailed Background]
Two to three sentences of more detailed background, comprehensible to scientists in related disciplines.

[3. General Problem]
One sentence clearly stating the general problem being addressed by this particular study.

[4. Main Result]
One sentence summarizing the main result (with the words “here we show” or their equivalent).

[5. Results with Comparisons]
Two or three sentences explaining what the main result reveals in direct comparison to what was thought to be the case previously, or how the main result adds to previous knowledge.

[6. General Context]
Place your results in a broader context in 1-2 sentences. This should link your findings to the wider field of study.

[7. Broader Perspective]
Two or three sentences to provide a broader perspective, readily comprehensible to a scientist in any discipline, may be included in the first paragraph if the editor considers that the accessibility of the paper is significantly enhanced by their inclusion. Under these circumstances, the length of the paragraph can be up to 300 words. (This example is 190 words without the final section, and 250 words with it).
----------------
TEMPLATE ENDS
----------------

For example, the abstract below is well-written, following the provided template.

----------------
EXAMPLE STARTS
----------------
[START of 1. Basic Introduction] During cell division, mitotic spindles are assembled by microtubule-based motor proteins1,2. [END of 1. Basic Introduction ] [START of 2. Detailed Background] The bipolar organization of spindles is essential for proper segregation of chromosomes, and requires plus-end-directed homotetrameric motor proteins of the widely conserved kinesin-5 (BimC) family3. Hypotheses for bipolar spindle formation include the ‘push–pull mitotic muscle’ model, in which kinesin-5 and opposing motor proteins act between overlapping microtubules2,4,5. [END of 2. Detailed Background] [START of 3. General Problem] However, the precise roles of kinesin-5 during this process are unknown. [END of 3. General Problem] [START of 4. Main Result] Here we show that the vertebrate kinesin-5 Eg5 drives the sliding of microtubules depending on their relative orientation. [END of 4. Main Result] [START of 5. Results with Comparisons] We found in controlled in vitro assays that Eg5 has the remarkable capability of simultaneously moving at ∼20 nm s-1 towards the plus-ends of each of the two microtubules it crosslinks. For anti-parallel microtubules, this results in relative sliding at ∼40 nm s-1, comparable to spindle pole separation rates in vivo6. Furthermore, we found that Eg5 can tether microtubule plus-ends, suggesting an additional microtubule-binding mode for Eg5. [END of 5. Results with Comparisons] [START of 6. General Context] Our results demonstrate how members of the kinesin-5 family are likely to function in mitosis, pushing apart interpolar microtubules as well as recruiting microtubules into bundles that are subsequently polarized by relative sliding. [END of 6. General Context] [START of 7. Broader Perspective] We anticipate our assay to be a starting point for more sophisticated in vitro models of mitotic spindles. For example, the individual and combined action of multiple mitotic motors could be tested, including minus-end-directed motors opposing Eg5 motility. Furthermore, Eg5 inhibition is a major target of anti-cancer drug development, and a well-defined and quantitative assay for motor function will be relevant for such developments. [END of 7. Broader Perspective]
----------------
EXAMPLE ENDS
----------------

Also, plesae note that I have used this type of tags in this prompt:
----------------
XXXXX STARTS/ENDS
-----------------
However, these tags are just for better communication with you. So, please do not include similar tags in your output.

Now, my draft is as follows. Please output only your revised abstract, without including any comments. Also, please return as a code block (``` tex\nYOUR REVISED ABSTRACT```).
-----------------
MY DRAFT STARTS
-----------------
PLACEHOLDER
-----------------
MY DRAFT ENDS
-----------------
