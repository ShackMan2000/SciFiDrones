using System;
using System.Collections.Generic;
using UnityEngine;


public class InputXR : MonoBehaviour
{

    [SerializeField] DroneMovement droneMovement;
        
    void Update()
    {
        Vector2 xzMovement = OVRInput.Get(OVRInput.Axis2D.PrimaryThumbstick, OVRInput.Controller.RTouch);

        droneMovement.SetForwardTarget(xzMovement.y);
        droneMovement.SetSidewaysTarget(xzMovement.x);
        
        
        float down = OVRInput.Get(OVRInput.Axis1D.PrimaryHandTrigger, OVRInput.Controller.RTouch);
        float up = OVRInput.Get(OVRInput.Axis1D.PrimaryIndexTrigger, OVRInput.Controller.RTouch);
        
        float upDown = up - down;
        
        droneMovement.SetVerticalTarget(upDown);
        
        float yaw = OVRInput.Get(OVRInput.Axis2D.PrimaryThumbstick, OVRInput.Controller.LTouch).x;
        
        droneMovement.SetYawTarget(yaw);
        
    }
}