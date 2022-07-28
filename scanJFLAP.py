################################################################################################
# This program can be used to generate a simplified intermediate representation for a DPDA
# created with JFLAP. Simply design your DPDA in JFLAP's Pushdown Automaton tool as you
# normally would, be sure to use only one accepting state, and save the resultant JFLAP
# source file as: ./user-files/source.jff, and call the main function in this file.
# The intermediate representation will be saved as ./user-files/source-lines.txt.
#
# The first two lines of the resultant source file will specify the names of the
# initial and accepting states, respectively, and the remaining lines will specify
# transition rules for the DPDA.
#
# Example: A source file for the language of properly nested parenthesis will look like:
# 0
# 3
# 0, _, _ -> $, 1
# 1, (, _ -> (, 1
# 1, ), ( -> _, 2
# 2, (, _ -> (, 1
# 2, ), ( -> _, 2
# 2, _, $ -> _, 3
#
#
################################################################################################


#import xml.etree.ElementTree as ET # This method is susceptible to XML security vulnerabilities.
import defusedxml.ElementTree as ET # Safer XML parsing.
import sys


# Scans an XML file and returns the root node of the ElementTree.
def scanXMLFile(xml_filename):
  tree = ET.parse(xml_filename)
  root = tree.getroot()
  return root


# Takes the root node of an ElementTree for a given JFLAP DPDA source file
# and returns a list containing the names of every state in the machine.
def scanStates(xml_root):

  # Get every element with a <state> tag.
  xml_states = xml_root.findall("automaton")[0].findall("state")
  
  # Temporary space for recording the initial and final states.
  initial = ""
  final = ""
  states = []

  # Extract the state names from the XML/JFLAP source code.
  for xml_state in xml_states:
    state_id = xml_state.attrib.get('id')

    initial_states = xml_state.findall("initial")
    final_states = xml_state.findall("final")

    if (initial_states != []):
      initial = state_id
    elif (final_states != []):
      final = state_id
    else:
      states.append(state_id)

  # Check that the machine specifies an initial state and an accepting state.
  if (initial == ""):
    print("Error -- Initial state not found")
    return []
  if (final == ""):
    print("Error -- Final state not found")
    return []

  # Return all the states, starting with the initial
  # state, and ending with the final state.
  return [initial] + states + [final]


# Takes the root node of an ElementTree for a given JFLAP DPDA source file
# and returns a list containing all of the machine's transition rules.
# The rules are sorted lexicographically by their source and destination states.
def scanTransitions(xml_root):

  # Get every element with a <transition> tag.
  xml_transitions = xml_root.findall("automaton")[0].findall("transition")
  transitions = []

  # Extract a rule from each transition.
  for xml_transition in xml_transitions:
    from_state = xml_transition.findall("from")[0].text
    to_state = xml_transition.findall("to")[0].text
    read = xml_transition.findall("read")[0].text
    pop = xml_transition.findall("pop")[0].text
    push = xml_transition.findall("push")[0].text

    # Convert all transitions into the epsilon symbol; '_'.
    if (read == None):
      read = '_'
    if (pop == None):
      pop = '_'
    if (push == None):
      push = '_'

    # Save the extracted transition specs.
    transitions.append((from_state, to_state, read, pop, push))

  # Lexicographically sort (from_state, to_state)
  transitions.sort()

  return transitions


# Generates and saves an intermediate representation of a DPDA from a JFLAP source file.
def main():

  # Reads the jflap source file, and extract the states and transition rules.
  jflap_xml_filename = "./user-files/source.jff"
  xml_root = scanXMLFile(jflap_xml_filename)
  states = scanStates(xml_root)
  transitions = scanTransitions(xml_root)
  result = ""

  # Saves the DPDA specifications to an external file.
  with open("./user-files/source-lines.txt", "r+") as text_file:

    #lines = []
    text_file.write(states[0] + '\n') # Record the initial state.
    text_file.write(states[-1] + '\n') # Record the final state.
    num_transitions = len(transitions)
    for i, transition in enumerate(transitions):
      # We need to swap the to_state and push values in each transition to match the haskell pda-maker.
      line = transition[0] + ", " + transition[2] + ", " + transition[3] + " -> " + transition[4] + ", " + transition[1]
      #lines.append(line)
      #print("line " + str(i) + ": " + line)
      text_file.write(line)
      if(i < num_transitions-1): text_file.write('\n')

    result = text_file.read()

  return result

main()
