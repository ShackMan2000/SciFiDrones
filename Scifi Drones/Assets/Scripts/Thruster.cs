using System.Collections.Generic;
using Sirenix.OdinInspector;
using UnityEngine;


public class Thruster : MonoBehaviour
{
    public bool IsLockedForYaw;

    public Transform Flapper;
    public Transform Ring;


    [SerializeField] List<Transform> vfxMeshes;




    [Button]
    public void AddScaleNoiseToVfxMeshes()
    {
        foreach (var vfxMesh in vfxMeshes)
        {
            float xNoise = Random.Range(-0.01f, 0.01f);
            float yNoise = Random.Range(-0.01f, 0.01f);
            float zNoise = Random.Range(-0.01f, 0.01f);
            
            vfxMesh.localScale += new Vector3(xNoise, yNoise, zNoise);
        }
    }

}