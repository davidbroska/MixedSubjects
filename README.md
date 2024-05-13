

# Data collection 

To simulate reponses with the provided code, follow these steps: 

1. Get Python 3.11 and PyTorch via `pip install torch==1.2` 
2. Run `pip install -r 1/DataCollection/requirements.txt`

To do: 
- Put Demographic information into system prompt, use language from Argyle et al. ([2023](https://dataverse.harvard.edu/file.xhtml?fileId=6711665&version=1.0))
- Check prompt result against the example from the original moral machine supplementary material
- What does is_in_car mean


## Parameters for function to generate the moral machine scenario

In the following, we describe the variables in the data by Awad, and what they imply for prompting the LLM with the code by Takemoto similar to Awad(x) -> Takemoto(x')

### ScenarioType 

- This denotes 
- Note that `ScenarioTypeStrict` or `ScenarioType` have the same values because we only use overlapping cases. This is in line with Awad (see their SI).

### PedPed

This variable describes whether respondents pits two groups of passengers against each other (those on the left side of the crossing versus those on the right) or passengers against pedestrians.
- If PedPed = 1, respondents have to decide between saving two groups of passengers. 
    - **If PedPed = 1, is_in_car=F**
- If PedPed = 0, respondents have to decide between saving pessengers or pedestrians
    - Then, is_in_car=T but there is a


is_in_car = True in Takemoto corresponds to PedPed = 1

### Barrier

This variable describes whether the scenario features a Barrier that the autonomous vehicle could crash into. 
- **PedPed=1, then Barrier=0** since there are only pedestrians.
- If PedPed=0, there are but passengers and pedestrians and the respondent can save one these two groups. In this case, and depending on which outcome the respondent chooses, Barrier=1 if the AV crashes into the Barrier or Barrier=0 if it does not.
- PedPed=1 pits pedestrians against pedestrians, and these groups are the same in the two cases. What varies between the left and the right side is which group dies as results of swerving or staying on track. 

### Intervention

- If Intervention=0, the passengers die as the result of staying on the lane
- If Intervention=1, the passengers die as the result of swerving onto the other lane

### CrossingSignal
This variable determines whether the scenario description means whether pedestrians cross a traffic light. 
- If Crossing Signal = 0, the description does not mention a traffic light. 
- If Crossing Signal = 1, pedestrians cross the street lawfully (green light).
- If Crossing Signal = 2, pedestrians cross the street unlawfully (red light).
- **If Crossing Signal=0, then is_law=False**.
- is_law=True in Takemoto corresponds to a description where predestrians either cross a street lawfully or unlawfully (Crossing Signal = 1 or 2). 
- If PedPed=0, there is only 1 crossing signal. If PedPed=1 there are two. 