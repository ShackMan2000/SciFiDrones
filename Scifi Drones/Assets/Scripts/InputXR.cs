using System;
using System.Collections.Generic;
using UnityEngine;


public class InputXR : MonoBehaviour
{

    [SerializeField] DroneMovement droneMovement;


    void Update()
    {
        float forwardInput = OVRInput.Get(OVRInput.Axis1D.PrimaryIndexTrigger);
    }
}