using System;
using System.Collections.Generic;
using Sirenix.OdinInspector;
using UnityEngine;


public class InputEditor : MonoBehaviour
{


    [SerializeField] DroneMovement droneMovement;
    
    
    // use sliders that set the stuff in the drone movement.


    [SerializeField, Range(-1f, 1f)]
    float forwardInput;
    
    [SerializeField, Range(-1f, 1f)]
    float sideInput;
    
    [SerializeField, Range(-1f, 1f)]
    float yawInput;
   
    
    void Update()
    {
        droneMovement.SetForwardTarget(forwardInput);
        droneMovement.SetSidewaysTarget(sideInput);
        droneMovement.SetYawTarget(yawInput);
    }
}